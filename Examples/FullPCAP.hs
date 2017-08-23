{-# LANGUAGE TypeFamilies
           , ScopedTypeVariables
           , DeriveDataTypeable
           , MultiParamTypeClasses
           , TypeSynonymInstances
           , TemplateHaskell
           , QuasiQuotes
           , FlexibleInstances
           , FlexibleContexts
           , RecordWildCards
           , NamedFieldPuns
           , OverloadedStrings #-}

module FullPCAP where

import Language.Pads.Padsc
import Language.Pads.Testing
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString as B
import qualified Language.Pads.Library.LittleEndian as LE
import qualified Language.Pads.Library.BigEndian as BE


hx2bs :: [Char] -> B.ByteString
hx2bs zs =
    let
        -- strip whitespace out
        stripws (' ' : xs) = stripws xs
        stripws ( x  : xs) = x : stripws xs
        stripws []         = []

        -- convert each character to a value
        toDigit x =
            if   elem x ['0'..'9']
            then (chrToWord8 x) - (chrToWord8 '0')
            else if   elem x ['a'..'f']
                 then (chrToWord8 x) - (chrToWord8 'a') + 10
                 else error "bad hex"

        -- make a Word8 out of each two characters
        pairs (a : b : rest) =
            fromIntegral ((16 * toDigit a) + toDigit b) : pairs rest
        pairs [] = []

    in B.pack $ pairs $ stripws zs


ports _ 53 = 53
ports 53 _  = 53
ports 123 _ = 123
ports _ 123 = 123
ports _ _ = 0

[pads|
    type BfList  (x :: Bits32) = Bytes <|  (fromIntegral x :: Int) |>

    data MACADDR = MACADDR {
        v1 :: Bits8 8,
        v2 :: Bits8 8,
        v3 :: Bits8 8,
        v4 :: Bits8 8,
        v5 :: Bits8 8,
        v6 :: Bits8 8
    }

    data NTP = NTP {
        ntp_li                  :: Bits8 2,
        ntp_vn                  :: Bits8 3,
        ntp_mode                :: Bits8 3,
        ntp_stratum             :: Bits8 8,
        ntp_poll                :: Bits8 8,
        ntp_precision           :: Bits8 8,
        ntp_root_delay          :: NTPS,
        ntp_root_dispersion     :: NTPS,
        ntp_reference_id        :: Bits32 32,
        ntp_reference_timestamp :: NTPTS,
        ntp_origin_timestamp    :: NTPTS,
        ntp_receive_timestamp   :: NTPTS,
        ntp_transmit_timestamp  :: NTPTS
        -- ,
        -- ntp_extension_field_1   :: NTPEF,
        -- ntp_extension_field_2   :: NTPEF,
        -- ntp_key_identifier      :: Bits32 32,
        -- ntp_dgst                :: Bits64 64
    }

    data NTPS = NTPS {
        ntps_seconds  :: Bits16 16,
        ntps_fraction :: Bits16 16
    }

    data NTPTS = NTPTS {
        ntpts_seconds  :: Bits32 32,
        ntpts_fraction :: Bits32 32
    }

    data NTPDF = NTPDF {
        ntpdf_era_number :: Bits32 32,
        ntpdf_era_offset :: Bits32 32,
        ntpdf_fraction   :: Bits64 64
    }

    type MACADDR' = partition MACADDR using none
    type NTPTS'   = partition NTPTS using none
    type NTPS'    = partition NTPS using none
    type NTPDF'   = partition NTPDF using none
    type NTP'     = partition NTP using none


    type PCAP = partition (PCAP_Header, [Packet]) using none

    data PCAP_Header = PCAP_Header {
        magic_number  :: LE.Int32,
        version_major :: LE.Int16,
        version_minor :: LE.Int16,
        this_zone     :: LE.Int32,
        sig_figs      :: LE.Int32,
        snap_len      :: LE.Int32,
        network       :: LE.Int32
    }

    data Packet = Packet {
        ts_sec   :: LE.Int32,
        ts_usec  :: LE.Int32,
        incl_len :: LE.Int32,
        orig_len :: LE.Int32,
        body     :: ETHERNET <| fromIntegral incl_len |>
        --body     :: BfList <| fromIntegral incl_len |>
    }


    data ETHERNET (n :: Bits32) = ETHERNET {
        destaddr_eth   :: MACADDR,
        sourceaddr_eth :: MACADDR,
        ethertype_val  :: Bits16 16,
        ether_body     :: IP4 <| n - 14 |>
    }

    data IP4 (n :: Bits32) = IP4 {
        ip4_version             :: Bits8 4,
        ip4_header_len          :: Bits8 4,
        ip4_type_of_service     :: Bits8 8,
        ip4_total_length        :: Bits16 16,
        ip4_identification      :: Bits16 16,
        ip4_a                   :: BitBool,
        ip4_b                   :: BitBool,
        ip4_c                   :: BitBool,
        ip4_fragment_offset     :: Bits16 13,
        ip4_time_to_live        :: Bits8 8,
        ip4_protocol            :: Bits8 8,
        ip4_header_checksum     :: Bits16 16,
        ip4_source_address      :: Bits32 32,
        ip4_destination_address :: Bits32 32,
        ip4_options             :: BfList <| (fromIntegral (ip4_header_len * 4) :: Bits32) - 20 |>,
        ip4_body                :: PROT <| (ip4_protocol,n - (4 * (fromIntegral ip4_header_len :: Bits32))) |>
    }

    data PROT (protocol :: Bits8, prot_len :: Bits32) =
      case protocol
        of 6  -> PROTCP    {tt:: TCP    <| prot_len |> }
         | 17 -> PROUDP    {uu:: UDP    <| prot_len |>}
         | _  -> OTHERWISE {oo:: BfList <| prot_len |>}

    data TCP (n :: Bits32) = TCP {
        tcp_source_port           :: Bits16 16,
        tcp_destination_port      :: Bits16 16,
        tcp_sequence_number       :: Bits32 32,
        tcp_acknowledgment_number :: Bits32 32,
        tcp_offset                :: Bits8 4,
        tcp_reserved              :: Bits8 6,
        tcp_u                     :: BitBool,
        tcp_a                     :: BitBool,
        tcp_p                     :: BitBool,
        tcp_r                     :: BitBool,
        tcp_s                     :: BitBool,
        tcp_f                     :: BitBool,
        tcp_window                :: Bits16 16,
        tcp_checksum              :: Bits16 16,
        tcp_urgent_pointer        :: Bits16 16,
        tcp_options               :: BfList <| 32 * ((fromIntegral tcp_offset) - 5) |>,
        tcp_data                  :: BfList <| (n - 20) - (32 * ((fromIntegral tcp_offset )-5)) |>
    }

    data UDP (n:: Bits32) = UDP {
        udp_source_port :: Bits16 16,
        udp_dest_port   :: Bits16 16,
        udp_length      :: Bits16 16,
        udp_checksum    :: Bits16 16,
        udp_body        :: UDPPORT <| ( (ports (fromIntegral udp_source_port :: Bits32) (fromIntegral udp_dest_port :: Bits32)), n - 8) |>
    }

    data UDPPORT (udp_port :: Bits32, n :: Bits32) =
      case udp_port
        of 53  -> DNSP       {dnsp:: BfList <| n |>}
         | 123 -> NTPP       {ntpp:: BfList <| n |>}
         | _   -> OTHERWISEP {otherwisep :: BfList <| n |>}
|]

((h, ps), md) = unsafePerformIO $ parseFileWith pCAP_parseM "data/NTP_sync.pcap"

((h2, ps2), md2) = unsafePerformIO $ parseFileWith pCAP_parseM "data/another.pcap"

test = runTestTT (TestList tests)

tests = [ TestLabel "macaddr" macaddr_test
        , TestLabel "ntpts" ntpts_test
        , TestLabel "ntps" ntps_test
        , TestLabel "ntpdf" ntpdf_test
        , TestLabel "ntp" ntp_test
        ]

macaddr_input = "10 0f 23 ad b3 4c"
macaddr_result = parseByteStringInput mACADDR'_parseM (hx2bs macaddr_input)
macaddr_expects =
    (MACADDR {
        v1 = 16,
        v2 = 15,
        v3 = 35,
        v4 = 173,
        v5 = 179,
        v6 = 76
    }, 0, "")
macaddr_test = mkTestCase "macaddr" macaddr_expects macaddr_result

ntpts_input = "c50204ecec42ee92"
ntpts_result = parseByteStringInput nTPTS'_parseM (hx2bs ntpts_input)
ntpts_expects =
    (NTPTS {
        ntpts_seconds  = 3305243884,
        ntpts_fraction = 3963809426
    }, 0, "")
ntpts_test = mkTestCase "ntpts" ntpts_expects ntpts_result

ntps_input = "4a30 94fe"
ntps_result = parseByteStringInput nTPS'_parseM (hx2bs ntps_input)
ntps_expects =
    (NTPS {
        ntps_seconds  = 18992,
        ntps_fraction = 38142
    }, 0, "")
ntps_test = mkTestCase "ntps" ntps_expects ntps_result

ntpdf_input = "54a9002b 3e12fff4 0ab0173d492b029c"
ntpdf_result = parseByteStringInput nTPDF'_parseM (hx2bs ntpdf_input)
ntpdf_expects =
    (NTPDF {
        ntpdf_era_number = 1420361771,
        ntpdf_era_offset = 1041432564,
        ntpdf_fraction   = 770141088268354204
    }, 0, "")
ntpdf_test = mkTestCase "ntpdf" ntpdf_expects ntpdf_result

ntp_input = "1a020aef00000f7a000776dd11fe0031c4fae6e5108637bdc50204ecec42ee92c50204ebd937d1fec50204ebd93dea46"
ntp_result = parseByteStringInput nTP'_parseM (hx2bs ntp_input)
ntp_expects =
    (NTP {
        ntp_li = 0,
        ntp_vn = 3,
        ntp_mode = 2,
        ntp_stratum = 2,
        ntp_poll = 10,
        ntp_precision = 239,
        ntp_root_delay =
            NTPS {ntps_seconds = 0, ntps_fraction = 3962},
        ntp_root_dispersion =
            NTPS {ntps_seconds = 7, ntps_fraction = 30429},
        ntp_reference_id = 301858865,
        ntp_reference_timestamp =
            NTPTS {ntpts_seconds = 3304777445, ntpts_fraction = 277231549},
        ntp_origin_timestamp =
            NTPTS {ntpts_seconds = 3305243884, ntpts_fraction = 3963809426},
        ntp_receive_timestamp =
            NTPTS {ntpts_seconds = 3305243883, ntpts_fraction = 3644314110},
        ntp_transmit_timestamp =
            NTPTS {ntpts_seconds = 3305243883, ntpts_fraction = 3644713542}
    }, 0, "")
ntp_test = mkTestCase "ntp" ntp_expects ntp_result
