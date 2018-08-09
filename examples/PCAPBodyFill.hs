{-# LANGUAGE DeriveDataTypeable
           , DeriveGeneric
           , DeriveLift
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , QuasiQuotes
           , ScopedTypeVariables
           , TemplateHaskell
           , TypeFamilies
           , TypeSynonymInstances
           , UndecidableInstances #-}

module PCAPBodyFill where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import qualified Data.ByteString.Char8 as B
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Word
import           System.IO.Unsafe

import Language.Pads.Padsc

-- Generation logic: generate as many packets as are needed to send the given
-- data (in this case a webpage), then go back through and fill their random
-- bodies with equally-sized pieces of the meaningful data - avoids changing
-- length fields in the packet (inclLen, ipv4TotLen) except for the last packet

-- Potential easy change: generate packets not all the same size, fix lengths
-- after the fact - should be a small change in fillBody and the original
-- generator for inclLen

-- See PCAPBodyGen.hs for an alternate approach


fI :: (Num b, Integral a) => a -> b
fI = fromIntegral

mtu :: Num a => a
mtu = 1460

[pads|
  type Bytes' (x :: Int) = Bytes <| max 0 x |>

  type PCAP = partition (PCAPHeader, Packets) using none

  data PCAPHeader = PCAPHeader {
      pchMagicNum   :: Bits32 32 generator <| return 0xa1b2c3d4 |>,
      pchVersionMaj :: Bits16 16 generator <| return 2 |>,
      pchVersionMin :: Bits16 16 generator <| return 4 |> ,
      pchThisZone   :: Bits32 32 generator <| return 0 |>,
      pchSigFigs    :: Bits32 32 generator <| return 0 |>,
      pchSnapLen    :: Bits32 32,
      pchNetwork    :: Bits32 32 generator <| return 1 |>
  }

  data Packets = Packets { ps :: [Packet] generator ps_genM }

  data Packet = Packet {
      tsSec   :: Bits32 32 generator <| liftIO $ floor <$> getPOSIXTime |>,
      tsUsec  :: Bits32 32 generator <| randNumBound 999999999 |>,
      inclLen :: Bits32 32 generator <| return $ mtu + 54 |>,
      origLen :: Bits32 32 generator <| return inclLen |>,
      body    :: Ethernet inclLen
  }

  data Ethernet (inclLen :: Bits32) = Ethernet {
    ethDst     :: MacAddr,
    ethSrc     :: MacAddr,
    ethType    :: Bits16 16 generator <| return 2048 |>,
    ethPayload :: EthPayload <| (ethType, inclLen) |>
  }

  data MacAddr = MacAddr {
    constrain m1 :: Bits8 8 where <| m1 `mod` 2 == 0 |>,
    m2           :: Bits8 8,
    m3           :: Bits8 8,
    m4           :: Bits8 8,
    m5           :: Bits8 8,
    m6           :: Bits8 8
  }

  data EthPayload (ethType :: Bits16, inclLen :: Bits32) = case ethType of
    2048 -> IPV4 {
      ipv4Version  :: Bits8 4 generator <| return 4 |>,
      ipv4IHL      :: Bits8 4 generator <| return 5 |>,
      ipv4DSCP     :: Bits8 6,
      ipv4ECN      :: Bits8 2,
      ipv4TotLen   :: Bits16 16 generator <| return (max 0 $ (fI inclLen) - 14) |>,
      ipv4ID       :: Bits16 16,
      ipv4Flags    :: IPV4Flags,
      ipv4FragOff  :: Bits16 13 generator <| return 0 |>,
      ipv4TTL      :: Bits8 8,
      ipv4Protocol :: Bits8 8 generator <| return 6 |>,
      ipv4Cksum    :: Bits16 16,
      ipv4Src      :: Bits32 32,
      ipv4Dst      :: Bits32 32,
      ipv4Opts     :: Bytes <| 4 * (max 0 $ (fI ipv4IHL) - 5) |>,
      ipv4Payload  :: IPV4Payload <| (ipv4Protocol, ipv4IHL, ipv4TotLen) |>
    }

  data IPV4Flags = IPV4Flags {
    ipv4Res :: BitBool generator <| return False |>,
    ipv4DF  :: BitBool,
    ipv4MF  :: BitBool generator <| return False |>
  }

  data IPV4Payload (prot :: Bits8, ipv4IHL :: Bits8, totLen :: Bits16) = case prot of
    6 -> TCP {
      tcpSrc      :: Bits16 16 generator <| return 80 |>,
      tcpDst      :: Bits16 16,
      tcpSeq      :: Bits32 32,
      tcpAck      :: Bits32 32,
      tcpOffset   :: Bits8 4 generator <| return 5 |>,
      tcpReserved :: Bits8 3 generator <| return 0 |>,
      tcpFlags    :: TCPFlags,
      tcpWindow   :: Bits16 16,
      tcpCksum    :: Bits16 16,
      tcpUrgPtr   :: Bits16 16,
      tcpOptions  :: Bytes <| 4 * (max 0 $ (fI tcpOffset) - 5) |>,
      tcpPayload  :: Bytes' <| (fI totLen) - (fI $ (tcpOffset * 4) + (ipv4IHL * 4)) |>
    }

  data TCPFlags = TCPFlags {
    tcpNS  :: BitBool,
    tcpCWR :: BitBool,
    tcpECE :: BitBool,
    tcpURG :: BitBool,
    tcpACK :: BitBool,
    tcpPSH :: BitBool,
    tcpRST :: BitBool generator <| return False |>,
    tcpSYN :: BitBool,
    tcpFIN :: BitBool
  }
|]


-- | One-stop shop for packet list generation - working at the level of a list
-- of packets, we simply call down to packet_genM then map over its results to
-- perform our filling
ps_genM :: PadsGen [Packet]
ps_genM = do
  bs <- liftIO $ B.pack <$> minify <$> readFile "data/galois.html"
  let padding = B.replicate (mtu - (B.length bs `mod` mtu)) '\NUL'
  let bs' = B.append bs padding
  ps <- replicateM (B.length bs' `div` mtu) packet_genM
  return $ fillBodies bs ps

  where
    -- | Uses State monad to keep track of what/how many bytes/packets are left
    fillBodies :: B.ByteString -> [Packet] -> [Packet]
    fillBodies bytes packets = do
      fst $ runState (mapM fillBody packets) (bytes, packets)

    -- | Construct a modified packet, changing only the body (and the lengths of
    -- the last packet). Would be cleaner with lenses probably
    fillBody :: Packet -> State (B.ByteString, [Packet]) Packet
    fillBody p = do
      (bs, ps) <- get
      let nextBytes = B.take mtu bs
      let restBytes = B.drop mtu bs
      let nextPacket  = head ps
      let restPackets = tail ps
      put (restBytes, restPackets)

      let tcpPayload'  = nextBytes
      let ipv4Payload' = (ipv4Payload (ethPayload (body p))) { tcpPayload = tcpPayload' }
      let ethPayload'  = (ethPayload (body p)) { ipv4Payload = ipv4Payload' }
      let body'        = (body p) { ethPayload = ethPayload' }
      let p' = p { body = body' {
                     ethPayload = ethPayload' {
                       ipv4Payload = ipv4Payload' {
                         tcpPayload = tcpPayload' } } } }

      if B.length nextBytes < mtu
        then let
          inclLen'    = fromIntegral $ B.length nextBytes + 54
          ipv4TotLen' = fromIntegral $ B.length nextBytes + 40
          in return p' { inclLen = inclLen', origLen = inclLen', body = body' {
                           ethPayload = ethPayload' {
                             ipv4TotLen = ipv4TotLen' } } }
        else return p'

-- | Small procedure to remove unnecessary HTML formatting spaces
minify :: String -> String
minify = unlines                  .
         map (dropWhile (== ' ')) .
         filter (any (/= ' '))    .
         lines

-- | Write a generated PCAP, as well as the HTML from all its packets - can be
-- opened to (visually, roughly) ensure no data has been lost/the HTML hasn't
-- been corrupted
writePCAP :: IO PCAP
writePCAP = do
  pcap <- runPadsGen pCAP_genM
  B.writeFile "data/test.pcap" $ (fromChunks . fromCL . pCAP_serialize) pcap
  let bs = map (tcpPayload . ipv4Payload . ethPayload . body) ((ps . snd) pcap)
  B.writeFile "data/maybeGalois.html" (B.concat bs)
  return pcap
