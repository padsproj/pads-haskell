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

module PCAPBodyGen where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import qualified Data.ByteString.Char8 as B
import           Data.IORef
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Word
import           System.IO.Unsafe

import Language.Pads.Padsc

-- Generation logic: don't generate random bodies, make the custom tcpPayload
-- generator read from an IORef acting as a global variable, taking as many
-- bytes as possible - inclLen generator reads from the same ref to ensure
-- correct lengths at the packet header stage, which will propagate down through
-- the packet

-- See PCAPBodyFill.hs for an alternate approach


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
      inclLen :: Bits32 32 generator inclLen_genM,
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
      tcpPayload  :: Bytes' <| (fI totLen) - (fI $ (tcpOffset * 4) + (ipv4IHL * 4)) |> generator tcpPayload_genM
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

-- | Global-variable-acting reference to data to fill bodies with. Tuple is
-- actual bytes of data, number of bytes of data, and desired length of packet
-- bodies
{-# NOINLINE ref #-}
ref :: IORef (B.ByteString, Int, Int)
ref = unsafePerformIO $ do
  bs <- B.pack <$> minify <$> readFile "data/galois.html"
  newIORef (bs, B.length bs, 1460)

ps_genM :: PadsGen [Packet]
ps_genM = do
  (_, availLen, desiredLen) <- liftIO $ readIORef ref
  case availLen `mod` desiredLen of
    0 -> replicateM      (availLen `div` desiredLen)  packet_genM
    _ -> replicateM (1 + (availLen `div` desiredLen)) packet_genM

-- | As long as possible, limited by availLen from ref - doesn't update ref,
-- that's tcpPayload_genM's job
inclLen_genM :: PadsGen Bits32
inclLen_genM = do
  (_, availLen, desiredLen) <- liftIO $ readIORef ref
  return $ 54 + (fromIntegral $ min availLen desiredLen)

-- | Grab however much data possible from the ref - put back in the ref the
-- updated state after the grab. After the last bytes are taken, reset the ref
-- to allow for repeated generation
tcpPayload_genM :: PadsGen Bytes'
tcpPayload_genM = do
  (bs, availLen, desiredLen) <- liftIO $ readIORef ref
  if availLen >= desiredLen
    then do
      liftIO $ writeIORef ref (B.drop desiredLen bs, availLen - desiredLen, desiredLen)
      return $ B.take desiredLen bs
    else do
      -- resetting the ref
      bs' <- liftIO $ B.pack <$> minify <$> readFile "data/galois.html"
      liftIO $ writeIORef ref (bs', B.length bs', 1460)
      return $ B.take availLen bs

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
