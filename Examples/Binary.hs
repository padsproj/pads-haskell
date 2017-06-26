{-# LANGUAGE TypeFamilies, TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}
module Examples.Binary where
import Language.Pads.Padsc
import Language.Pads.Library.LittleEndian as LE
import qualified Language.Pads.Library.BigEndian as BE
import qualified Language.Pads.Library.Native as Native
import Language.Pads.Testing
import System.IO.Unsafe (unsafePerformIO)

test = runTestTT $ TestList tests

tests =         [TestLabel "Calls" call_test
                ,TestLabel "Calls" callNoRec_test
                ,TestLabel "BaseTypes" testRec_test]


[pads| data Call = Call
            { onpa  :: Int32, obase :: LE.Int32
            , dpna  :: Int32, dbase :: Int32
            , con   :: Int32
            , dur   :: Int32 }

       type Calls = [Call | EOR] terminator EOF
       type CallsB = partition Calls using <| bytes 24 |>

       type CallsNoRec = [Call] terminator EOF
       type CallsNoRecInstall = partition CallsNoRec using none
|]

call_input_file = "data/calls10.b"
call_result :: (Calls, Calls_md) = unsafePerformIO $ parseFileWith callsB_parseM call_input_file
call_expect = ([Call {onpa = 221, obase = 2360199, dpna = 936, dbase = 3615299, con = 96977709, dur = 71},Call {onpa = 221, obase = 2360399, dpna = 855, dbase = 5445599, con = 96978527, dur = 126},Call {onpa = 221, obase = 2390199, dpna = 974, dbase = 4298199, con = 96980122, dur = 990},Call {onpa = 221, obase = 2390399, dpna = 855, dbase = 5404599, con = 96976885, dur = 35},Call {onpa = 222, obase = 2332099, dpna = 426, dbase = 6814599, con = 96977173, dur = 4850},Call {onpa = 222, obase = 2332099, dpna = -15, dbase = 9999999, con = 96978457, dur = 533},Call {onpa = 222, obase = 2340599, dpna = 855, dbase = 8442799, con = 96980047, dur = 8},Call {onpa = 222, obase = 2341299, dpna = 855, dbase = 7081599, con = 96979104, dur = 11},Call {onpa = 223, obase = 2340099, dpna = 855, dbase = 2484499, con = 96977739, dur = 564},Call {onpa = 223, obase = 2340199, dpna = -15, dbase = 9999999, con = 96979345, dur = 143}],0)
call_test = mkFileTestCase "Call" call_expect call_result

callNoRec_result = unsafePerformIO $ parseFileWith callsNoRecInstall_parseM call_input_file
callNoRec_test = mkFileTestCase "CallNoRec" call_expect callNoRec_result

binary_input_file = "data/binary"
word32sbh_result :: (Word32, Base_md) = unsafePerformIO $ parseFileWith BE.word32_parseM binary_input_file
word32sbh_expect = (2864434397,0 )

word32sbl_result :: (Word32, Base_md) = unsafePerformIO $ parseFileWith LE.word32_parseM binary_input_file
word32sbl_expect = (3721182122,0)

{- on a little endian machine -}
word32_result :: (Word32, Base_md) = unsafePerformIO $ parseFileWith Native.word32_parseM binary_input_file
word32_expect = (3721182122,0)


int32sbh_result :: (Int32, Base_md) = unsafePerformIO $ parseFileWith BE.int32_parseM binary_input_file
int32sbh_expect = (-1430532899,0)

int32sbl_result :: (Int32, Base_md) = unsafePerformIO $ parseFileWith LE.int32_parseM binary_input_file
int32sbl_expect = (-573785174,0 )

{- on a little endian machine -}
int32_result :: (Int32, Base_md) = unsafePerformIO $ parseFileWith Native.int32_parseM binary_input_file
int32_expect = (-573785174,0 )


[pads| data TestRec = TestRec
             { w8  :: Word8
             , i8  :: Int8
             , w16 :: Word16
             , i16 :: Int16
             , j16 :: Int16
             , w32 :: Word32
             , i32 :: Int32
             }  |]

testRec_input_file = "data/binTest.b"
testRec_result :: (TestRec, TestRec_md) = unsafePerformIO $ parseFileWith testRec_parseM testRec_input_file
testRec_expect = (TestRec {w8 = 25, i8 = 23, w16 = 300, i16 = -400, j16 = 899, w32 = 9876, i32 = -98744},0)
testRec_test = mkFileTestCase "BinaryBaseTypes" testRec_expect testRec_result
