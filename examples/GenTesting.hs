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

module GenTesting where

import           Data.Bits
import qualified Data.ByteString as B
import           Data.Maybe (fromJust)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Word

import           Control.Monad
import           Numeric (showHex, readHex)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           System.IO.Unsafe (unsafePerformIO)
import           Test.HUnit hiding (test)

import           Language.Pads.Padsc
import qualified Language.Pads.Library.LittleEndian as LE
import qualified Language.Pads.Library.BigEndian    as BE

sampleSize = 100 -- used for "cycle" testing - generate, serialize, parse

--------------------------------------------------------------------------------
-- Unit testing of serialization of pure/primitive PADS types

charTest_name = "Char"
charTest_expected = [chrToWord8 'a']
charTest_got = fromChunks $ fromCL $ char_serialize 'a'
charTest = TestCase (charTest_expected @=? charTest_got)

charCycleTest_name = "Char Cycle"
charCycleTest = do
  cs <- replicateM sampleSize (runGen char_genM)
  let cs_serialized = map ((map word8ToChr) . fromChunks . fromCL . char_serialize) cs
  let cs_parsed = map (fst . fst . (parseStringInput char_parseM)) cs_serialized
  return $ cs == cs_parsed

intTest_name = "Int"
intTest_expected = [chrToWord8 '2']
intTest_got = fromChunks $ fromCL $ int_serialize 2
intTest = TestCase (intTest_expected @=? intTest_got)

intCycleTest_name = "Int Cycle"
intCycleTest = do
  xs <- replicateM sampleSize (runGen int_genM)
  let xs_serialized = map ((map word8ToChr) . fromChunks . fromCL . int_serialize) xs
  let xs_parsed = map (fst . fst . (parseStringInput int_parseM)) xs_serialized
  return $ xs == xs_parsed

-- Purely for parsers that ignore default newline record discipline
[pads| type Bits8N   = partition Bits8  8  using none
       type Bits16N  = partition Bits16 16 using none
       type Bits32N  = partition Bits32 32 using none
       type Bits64N  = partition Bits64 64 using none
       type BitBoolN = partition BitBool   using none |]
bits8Test_name = "Bits8"
bits8Test_expected = [98]
bits8Test_got = (fromChunks $ (fromCL $ ((bits8_serialize 8) 98)))
bits8Test = TestCase (bits8Test_expected @=? bits8Test_got)

bits8CycleTest_name = "Bits8 Cycle"
bits8CycleTest = do
  xs <- replicateM sampleSize (runGen (bits8_genM 8))
  let xs_serialized = map ((map word8ToChr) . fromChunks . fromCL . (bits8_serialize 8)) xs
  let xs_parsed = map (fst . fst . (parseStringInput bits8N_parseM)) xs_serialized
  return $ xs == xs_parsed

bits8MisalignedTest_name = "Bits8 Misaligned"
bits8MisalignedTest_expected = [224] -- representing 11100000 (or 7 << 5)
bits8MisalignedTest_got
  = (fromChunks $ (fromCL $ ((bits8_serialize 3) 7)))
bits8MisalignedTest
  = TestCase
      (bits8MisalignedTest_expected @=? bits8MisalignedTest_got)

bits16Test_name = "Bits16"
bits16Test_expected = [139,74]
bits16Test_got = (fromChunks $ (fromCL $ ((bits16_serialize 16) 35658)))
bits16Test = TestCase (bits16Test_expected @=? bits16Test_got)

bits16CycleTest_name = "Bits16 Cycle"
bits16CycleTest = do
  xs <- replicateM sampleSize (runGen (bits16_genM 16))
  let xs_serialized = map ((map word8ToChr) . fromChunks . fromCL . (bits16_serialize 16)) xs
  let xs_parsed = map (fst . fst . (parseStringInput bits16N_parseM)) xs_serialized
  return $ xs == xs_parsed

bits16MisalignedTest_name = "Bits16 Misaligned"
bits16MisalignedTest_expected = [224,192] -- representing 1110000011000000 (or 899 << 6)
bits16MisalignedTest_got
  = (fromChunks $ (fromCL $ ((bits16_serialize 10) 899)))
bits16MisalignedTest
  = TestCase
      (bits16MisalignedTest_expected @=? bits16MisalignedTest_got)

bits32Test_name = "Bits32"
bits32Test_expected = [128,128,128,129]
bits32Test_got = (fromChunks $ (fromCL $ ((bits32_serialize 32) 2155905153)))
bits32Test = TestCase (bits32Test_expected @=? bits32Test_got)

bits32CycleTest_name = "Bits32 Cycle"
bits32CycleTest = do
  xs <- replicateM sampleSize (runGen (bits32_genM 32))
  let xs_serialized = map ((map word8ToChr) . fromChunks . fromCL . (bits32_serialize 32)) xs
  let xs_parsed = map (fst . fst . (parseStringInput bits32N_parseM)) xs_serialized
  return $ xs == xs_parsed

bits64Test_name = "Bits64"
bits64Test_expected = [128,128,128,128,128,128,128,129]
bits64Test_got = (fromChunks $ (fromCL $ ((bits64_serialize 64) 9259542123273814145)))
bits64Test = TestCase (bits64Test_expected @=? bits64Test_got)

bits64CycleTest_name = "Bits64 Cycle"
bits64CycleTest = do
  xs <- replicateM sampleSize (runGen (bits64_genM 64))
  let xs_serialized = map ((map word8ToChr) . fromChunks . fromCL . (bits64_serialize 64)) xs
  let xs_parsed = map (fst . fst . (parseStringInput bits64N_parseM)) xs_serialized
  return $ xs == xs_parsed

bitBoolTest_name = "BitBool"
bitBoolTest_expected = [128]
bitBoolTest_got = (fromChunks . fromCL . bitBool_serialize) True
bitBoolTest = TestCase (bitBoolTest_expected @=? bitBoolTest_got)

bitBoolCycleTest_name = "BitBool Cycle"
bitBoolCycleTest = do
  bs <- replicateM sampleSize (runGen bitBool_genM)
  let bs_serialized = map ((map word8ToChr) . fromChunks . fromCL . bitBool_serialize) bs
  let bs_parsed = map (fst . fst . (parseStringInput bitBoolN_parseM)) bs_serialized
  return $ bs == bs_parsed

-- | Use BitField generator and serializer but BitField50 parser to avoid record
-- discipline problems
[pads| type BitField50 = partition BitField 50 using none |]
bitFieldCycleTest_name = "BitField Cycle"
bitFieldCycleTest = do
  xs <- replicateM sampleSize (runGen (bitField_genM 50))
  let xs_serialized = map ((map word8ToChr) . fromChunks . fromCL . (bitField_serialize 50)) xs
  let xs_parsed = map (fst . fst . bitField50_parseS) xs_serialized
  return $ xs == xs_parsed

bytesTest_name = "Bytes"
bytesTest_expected = [1, 2, 3, 4]
bytesTest_got
  = (fromChunks $ (fromCL $ ((bytes_serialize 4) $ (B.pack [1, 2, 3, 4]))))
bytesTest = TestCase (bytesTest_expected @=? bytesTest_got)

[pads| type MyStringC = StringC 'f' |]

myStringCTest_name = "MyStringC"
myStringCTest_expected
  = [CharChunk 't', CharChunk 'g', CharChunk 'i', CharChunk 'f']
myStringCTest_got = (fromCL $ (myStringC_serialize "tgi"))
myStringCTest
  = TestCase (myStringCTest_expected @=? myStringCTest_got)

myStringCCycleTest_name = "MyStringC Cycle"
myStringCCycleTest = do
  ss <- replicateM sampleSize (runGen myStringC_genM)
  let ss_serialized = map ((map word8ToChr) . fromChunks . fromCL . myStringC_serialize) ss
  let ss_parsed = map (fst . fst . myStringC_parseS) ss_serialized
  return $ ss == ss_parsed

-------------------------------------------------------------------------------
-- Unit testing of user-defined PADS types

-- Simple tuple with literals
[pads| type MyTuple = (Int,'c',Bits8 4,'d') |]

myTupleTest_name = "MyTuple"
myTupleTest_expected
  = [CharChunk '1', CharChunk 'c', (BinaryChunk 10) 4, CharChunk 'd']
myTupleTest_got = fromCL $ myTuple_serialize (1, 10)
myTupleTest
  = TestCase (myTupleTest_expected @=? myTupleTest_got)

myTupleCycleTest_name = "MyTuple Cycle"
myTupleCycleTest = do
  ts <- replicateM sampleSize (runGen myTuple_genM)
  let ts_serialized = map ((map word8ToChr) . fromChunks . fromCL . myTuple_serialize) ts
  let ts_parsed = map (fst . fst . myTuple_parseS) ts_serialized
  return $ ts == ts_parsed

-- Application of type to argument
[pads| type Byte = Bits8 8
       type ByteN = partition Byte using none |]

byteTest_name = "Byte"
byteTest_expected = [(BinaryChunk 254) 8]
byteTest_got = fromCL $ byte_serialize (254 :: Byte)
byteTest = TestCase (byteTest_expected @=? byteTest_got)

byteCycleTest_name = "Byte Cycle"
byteCycleTest = do
  bs <- replicateM sampleSize (runGen byte_genM)
  let bs_serialized = map ((map word8ToChr) . fromChunks . fromCL . byte_serialize) bs
  let bs_parsed = map (fst . fst . byteN_parseS) bs_serialized
  return $ bs == bs_parsed

-- PPartition (generation) with reference to previously defined type
[pads| type TwoBytes = (Byte, Byte)
       type TwoBytesP = partition TwoBytes using none |]

twoBytesTest_name = "TwoBytes"
twoBytesTest_expected = [(BinaryChunk 0) 8, (BinaryChunk 122) 8]
twoBytesTest_got = fromCL $ twoBytes_serialize ((0, 122) :: TwoBytes)
twoBytesTest
  = TestCase (twoBytesTest_expected @=? twoBytesTest_got)

twoBytesCycleTest_name = "TwoBytes Cycle"
twoBytesCycleTest = do
  ts <- replicateM sampleSize (runGen twoBytesP_genM)
  let ts_serialized = map ((map word8ToChr) . fromChunks . fromCL . twoBytes_serialize) ts
  let ts_parsed = map (fst . fst . twoBytesP_parseS) ts_serialized
  return $ ts == ts_parsed

-- Nested tuple
[pads| type TupleN = (Int, ',', (Int,':',Int), ';', Int) |]
nestedTupleTest_name = "Nested Tuple"
nestedTupleTest_expected
  = [CharChunk '1', CharChunk ',', CharChunk '2', CharChunk ':',
     CharChunk '3', CharChunk ';', CharChunk '4']
nestedTupleTest_got = fromCL $ tupleN_serialize (1, (2, 3), 4)
nestedTupleTest
  = TestCase (nestedTupleTest_expected @=? nestedTupleTest_got)

-- PConstrain serialization (simple) and generation (less simple)
[pads| type CString = constrain s :: StringFW 10 where <| take 2 s == "cc" |> |]
constrainedStringTest_name = "Constrained String"
constrainedStringTest_expected = [CharChunk 'c', CharChunk 'c']
constrainedStringTest_got
  = ((take 2) $ (fromCL $ (cString_serialize "ccjqnbfkwh")))
constrainedStringTest
  = TestCase
      (constrainedStringTest_expected @=? constrainedStringTest_got)

constrainedGenTest_name = "Constrained Generation"
constrainedGenTest = do
  s <- (runGen cString_genM)
  return $ take 2 s == "cc"

-- PLists of several forms
-- TODO: terminator LLen
[pads| type RegularList = [Bits8 8]
       type SepList     = [Bits8 8 | '|']
       type SepTermList = [Bits8 8 | '|'] terminator Char
       --type SepTermList2 = [Bits8 8 | '|'] terminator EOR |]

regularListTest_name = "RegularList"
regularListTest_expected = [(BinaryChunk 10) 8, (BinaryChunk 5) 8]
regularListTest_got
  = fromCL $ regularList_serialize [10, 5]
regularListTest
  = TestCase (regularListTest_expected @=? regularListTest_got)

sepListTest_name = "SepList"
sepListTest_expected
  = [(BinaryChunk 10) 8, CharChunk '|', (BinaryChunk 5) 8]
sepListTest_got = fromCL $ sepList_serialize [10, 5]
sepListTest
  = TestCase (sepListTest_expected @=? sepListTest_got)

sepTermListTest_name = "SepTermList"
sepTermListTest_expected
  = [(BinaryChunk 10) 8, CharChunk '|', (BinaryChunk 5) 8, CharChunk 'X']
sepTermListTest_got
  = fromCL $ sepTermList_serialize [10, 5]
sepTermListTest
  = TestCase (sepTermListTest_expected @=? sepTermListTest_got)

sepTermListBytesTest_name = "SepTermList Bytes"
sepTermListBytesTest_expected = [10, 124, 5, 88]
sepTermListBytesTest_got = fromChunks sepTermListTest_got
sepTermListBytesTest
  = TestCase (sepTermListBytesTest_expected @=? sepTermListBytesTest_got)

-- Run-of-the-mill record with sub-byte data
[pads| data Pixel = Pixel { a :: Bits16 9
                          , b :: Bits8 5
                          , c :: Bits8 5
                          , d :: Bits8 5
                          , pb_index :: Bits8 4
                          , pr_index :: Bits8 4 }

       type PixelNone = partition Pixel using none
       type Pixels = partition (Pixel, [Pixel]) using none |]

pixelTest_name = "Pixel"
pixelTest_expected
  = [(BinaryChunk 511) 9, (BinaryChunk 0) 5, (BinaryChunk 8) 5,
     (BinaryChunk 0) 5, (BinaryChunk 1) 4, (BinaryChunk 1) 4]
pixelTest_got = fromCL $ pixel_serialize ((((((Pixel 511) 0) 8) 0) 1) 1)
pixelTest
  = TestCase (pixelTest_expected @=? pixelTest_got)

pixelBytesTest_name = "Pixel Bytes"
pixelBytesTest_expected = [255, 129, 0, 17]
pixelBytesTest_got = fromChunks pixelTest_got
pixelBytesTest
  = TestCase (pixelBytesTest_expected @=? pixelBytesTest_got)

pixelCycleTest_name = "Pixel Cycle"
pixelCycleTest = do
  ps <- replicateM sampleSize (runGen pixel_genM)
  let ps_serialized = map ((map word8ToChr) . fromChunks . fromCL . pixel_serialize) ps
  let ps_parsed = map (fst . fst . pixelNone_parseS) ps_serialized
  return $ ps == ps_parsed

-- Constants in records
[pads| data Constants = Constants { var1 :: Int
                                  , "string"
                                  , 'c'
                                  , var2 :: StringFW 10 } |]

recordConstantsTest_name = "Record Constants"
recordConstantsTest_expected = map chrToWord8 "42stringchellothere"
recordConstantsTest_got
  = (fromChunks
       $ (fromCL $ (constants_serialize $ ((Constants 42) "hellothere"))))
recordConstantsTest
  = TestCase
      (recordConstantsTest_expected @=? recordConstantsTest_got)

constantsCycleTest_name = "Constants Cycle"
constantsCycleTest = do
  cs <- replicateM sampleSize (runGen constants_genM)
  let cs_serialized = map ((map word8ToChr) . fromChunks . fromCL . constants_serialize) cs
  let cs_parsed = map (fst . fst . constants_parseS) cs_serialized
  return $ cs == cs_parsed

-- Simple PValue example
[pads| data Foo = Foo { x :: Int, xSucc = value <| x + 1 |> :: Int }
                | Bar { x :: Int } |]

fooFooTest_name = "FooFoo"
fooFooTest_expected = [CharChunk '1']
fooFooTest_got = fromCL $ foo_serialize ((Foo 1) 2)
fooFooTest = TestCase (fooFooTest_expected @=? fooFooTest_got)

fooBarTest_name = "FooBar"
fooBarTest_expected = [CharChunk '0']
fooBarTest_got = fromCL $ foo_serialize (Bar 0)
fooBarTest = TestCase (fooBarTest_expected @=? fooBarTest_got)

-- Test branch constructors
[pads| data MyConstr a = MyConstr1 Int Char
                       | MyConstr2 Void
                       | MyConstr3 a "Hello"
                       | MyConstr4 |]

myConstr1WithArgsTest_name = "MyConstr1 (With Args)"
myConstr1WithArgsTest_expected
  = [CharChunk '1', CharChunk '2', CharChunk 'x']
myConstr1WithArgsTest_got
  = (fromCL $ (myConstr_serialize undefined ((MyConstr1 12) 'x')))
myConstr1WithArgsTest
  = TestCase (myConstr1WithArgsTest_expected @=? myConstr1WithArgsTest_got)

myConstr2NoArgsTest_name = "MyConstr2 (No Args)"
myConstr2NoArgsTest_expected = []
myConstr2NoArgsTest_got
  = (fromCL $ (myConstr_serialize undefined MyConstr2))
myConstr2NoArgsTest
  = TestCase (myConstr2NoArgsTest_expected @=? myConstr2NoArgsTest_got)

myConstr3TVArgsTest_name = "MyConstr3 (With Tyvar Args)"
myConstr3TVArgsTest_expected
  = [CharChunk '3',CharChunk 'H',CharChunk 'e',
     CharChunk 'l',CharChunk 'l',CharChunk 'o']
myConstr3TVArgsTest_got = (fromCL $ (myConstr_serialize int_serialize (MyConstr3 3)))
myConstr3TVArgsTest
  = TestCase
      (myConstr3TVArgsTest_expected @=? myConstr3TVArgsTest_got)

myConstr4NoArgsTest_name = "MyConstr4 (No Args)"
myConstr4NoArgsTest_expected
  = [CharChunk 'M', CharChunk 'y', CharChunk 'C',CharChunk 'o', CharChunk 'n',
     CharChunk 's', CharChunk 't', CharChunk 'r', CharChunk '4']
myConstr4NoArgsTest_got
  = (fromCL $ (myConstr_serialize undefined (MyConstr4)))
myConstr4NoArgsTest
  = TestCase (myConstr4NoArgsTest_expected @=? myConstr4NoArgsTest_got)

-- This test fails on account of odd parsing behavior - it's included here as a
-- cautionary tale but excluded from the list of tests
myConstrCycleTest_name = "MyConstr Cycle"
myConstrCycleTest = do
  cs <- replicateM sampleSize (runGen (myConstr_genM int_genM))
  let cs_serialized = map ((map word8ToChr) . fromChunks . fromCL . (myConstr_serialize int_serialize)) cs
  let cs_parsed = map (fst . fst . (myConstr_parseS int_parseM)) cs_serialized
  return $ cs == cs_parsed

-- Test use of type variables and newtype
[pads| data MyList a = MyCons a (MyList a)
                     | MyNil Void

       newtype NT = NT Int |]

myListEmptyTest_name = "MyList Empty"
myListEmptyTest_expected = []
myListEmptyTest_got
  = (fromCL $ ((myList_serialize undefined) MyNil))
myListEmptyTest
  = TestCase (myListEmptyTest_expected @=? myListEmptyTest_got)

myListNonemptyTest_name = "MyList Nonempty"
myListNonemptyTest_expected
  = [CharChunk 'f', CharChunk 'o', CharChunk 'o']
myListNonemptyTest_got
  = (fromCL
       $ ((myList_serialize char_serialize)
            ((MyCons 'f') ((MyCons 'o') ((MyCons 'o') MyNil)))))
myListNonemptyTest
  = TestCase (myListNonemptyTest_expected @=? myListNonemptyTest_got)

myListCycleTest_name = "MyList Cycle"
myListCycleTest = do
  ls <- replicateM sampleSize (runGen (myList_genM char_genM))
  let ls_serialized = map ((map word8ToChr) . fromChunks . fromCL . (myList_serialize char_serialize)) ls
  let ls_parsed = map (fst . fst . (myList_parseS char_parseM)) ls_serialized
  return $ ls == ls_parsed

nTTest_name = "NewType"
nTTest_expected = [CharChunk '3']
nTTest_got = (fromCL $ (nT_serialize (NT 3)))
nTTest = TestCase (nTTest_expected @=? nTTest_got)

nTCycleTest_name = "NewType Cycle"
nTCycleTest = do
  nts <- replicateM sampleSize (runGen nT_genM)
  let nts_serialized = map ((map word8ToChr) . fromChunks . fromCL . nT_serialize) nts
  let nts_parsed = map (fst . fst . nT_parseS) nts_serialized
  return $ nts == nts_parsed

-- PSwitch - should desugar to simple record
[pads| data SwitchRec (x :: Int) = case <| x `mod` 2 |>
                                     of 0 -> Even
                                      | 1 -> Odd

       data SwitchTest = SwitchTest { arg :: Int, parity :: SwitchRec arg } |]

switchTest_name = "Switch"
switchTest_expected = map chrToWord8 "150119154Even"
switchTest_got
  = (fromChunks
       $ (fromCL
            $ (switchTest_serialize
                 $ SwitchTest {arg = 150119154, parity = Even})))
switchTest
  = TestCase (switchTest_expected @=? switchTest_got)

switchCycleTest_name = "Switch Cycle"
switchCycleTest = do
  ss <- replicateM sampleSize (runGen switchTest_genM)
  let ss_serialized = map ((map word8ToChr) . fromChunks . fromCL . switchTest_serialize) ss
  let ss_parsed = map (fst . fst . switchTest_parseS) ss_serialized
  return $ ss == ss_parsed

-- Test references to previously defined variables
[pads| data Dependent = Dependent { f :: Bits8 8
                                  , g :: Bytes <|fromIntegral f|> } |]

dependentSerTest_name = "Dependent Serialization"
dependentSerTest_expected
  = [BinaryChunk 7 8] ++ replicate 7 (CharChunk 'c')
dependentSerTest_got
  = fromCL $ dependent_serialize (Dependent 7 (B.pack [99,99,99,99,99,99,99]))
dependentSerTest
  = TestCase (dependentSerTest_expected @=? dependentSerTest_got)

dependentGenTest_name = "Dependent Generation"
dependentGenTest_invariant = do
  deps <- replicateM sampleSize (runGen dependent_genM)
  return $ all (== True)
    (map (\dep -> (fromIntegral $ f dep) == (B.length $ g dep)) deps)
dependentGenTest
  = TestCase (assert dependentGenTest_invariant)

-- Test paramaterization of structures
[pads| data Param1 = Param1 { p1 :: Bits8 8
                            , p2 :: Param2 p1}

       data Param2 (param :: Bits8) = Param2 { p3 :: Bytes <|fromIntegral param|> } |]

paramSerTest_name = "Parameterized Serialization"
paramSerTest_expected
  = [BinaryChunk 3 8, CharChunk 'c', CharChunk 'c', CharChunk 'c']
paramSerTest_got
  = (fromCL
       $ (param1_serialize ((Param1 3) (Param2 (B.pack [99, 99, 99])))))
paramSerTest
  = TestCase (paramSerTest_expected @=? paramSerTest_got)

paramGenTest_name = "Parameterized Generation"
paramGenTest_invariant = do
  params <- replicateM sampleSize (runGen param1_genM)
  return $ all (== True)
    (map (\par -> (fromIntegral $ p1 par) == (B.length (p3 (p2 par)))) params)
paramGenTest
  = TestCase (assert paramGenTest_invariant)

-- Obtain with custom generator (non-isomorphic types)
[pads| type Hex = obtain Int from String using <| (hexStrToInt, intToHexStr) |> generator absInt_genM |]

hexStrToInt :: Span -> (String, String_md) -> (Int, Int_md)
hexStrToInt _ (s, md) = ((fst . (!! 0) . readHex) s, md)

intToHexStr :: (Int, Int_md) -> (String, String_md)
intToHexStr (i, md) = (showHex i "", md)

absInt_genM :: PadsGen Int
absInt_genM = abs <$> randNum

hexObtainTest_name = "Hex Obtain"
hexObtainTest_expected
  = [CharChunk 'a', CharChunk 'b', CharChunk '1', CharChunk '2']
hexObtainTest_got = (fromCL $ (hex_serialize 43794))
hexObtainTest
  = TestCase (hexObtainTest_expected @=? hexObtainTest_got)

hexCycleTest_name = "Hex Cycle"
hexCycleTest = do
  hs <- replicateM sampleSize (runGen hex_genM)
  let hs_serialized = map ((map word8ToChr) . fromChunks . fromCL . hex_serialize) hs
  let hs_parsed = map (fst . fst . hex_parseS) hs_serialized
  return $ hs == hs_parsed

-- Test serialization from obtain decs (relies on their conversion functions),
-- along with creation of qualified serializer names (e.g. LE.int16_serialize)
[pads| type MyLEInt8 = LE.Int8 |]
littleInt8Test_name = "LE Int8"
littleInt8Test_expected = [CharChunk (word8ToChr 1)]
littleInt8Test_got = fromCL $ myLEInt8_serialize 1
littleInt8Test = TestCase (littleInt8Test_expected @=? littleInt8Test_got)

[pads| type MyLEInt16 = LE.Int16 |]
littleInt16Test_name = "LE Int16"
littleInt16Test_expected = [CharChunk (word8ToChr 0), CharChunk (word8ToChr 1)]
littleInt16Test_got = fromCL $ myLEInt16_serialize 256
littleInt16Test = TestCase (littleInt16Test_expected @=? littleInt16Test_got)

[pads| type MyLEInt32 = LE.Int32 |]
littleInt32Test_name = "LE Int32"
littleInt32Test_expected
  = [CharChunk (word8ToChr 1), CharChunk (word8ToChr 1),
     CharChunk (word8ToChr 1), CharChunk (word8ToChr 0)]
littleInt32Test_got = fromCL $ myLEInt32_serialize 65793
littleInt32Test = TestCase (littleInt32Test_expected @=? littleInt32Test_got)

[pads| type MyBEInt8 = BE.Int8 |]
bigInt8Test_name = "BE Int8"
bigInt8Test_expected = [CharChunk (word8ToChr 253)]
bigInt8Test_got = fromCL $ myBEInt8_serialize 253
bigInt8Test = TestCase (bigInt8Test_expected @=? bigInt8Test_got)

[pads| type MyBEInt16 = BE.Int16 |]
bigInt16Test_name = "BE Int16"
bigInt16Test_expected = [CharChunk (word8ToChr 1), CharChunk (word8ToChr 0)]
bigInt16Test_got = fromCL $ myBEInt16_serialize 256
bigInt16Test = TestCase (bigInt16Test_expected @=? bigInt16Test_got)

[pads| type MyBEInt32 = BE.Int32 |]
bigInt32Test_name = "BE Int32"
bigInt32Test_expected
  = [CharChunk (word8ToChr 0), CharChunk (word8ToChr 1),
     CharChunk (word8ToChr 1), CharChunk (word8ToChr 1)]
bigInt32Test_got = fromCL $ myBEInt32_serialize 65793
bigInt32Test = TestCase (bigInt32Test_expected @=? bigInt32Test_got)

fI = fromIntegral

[pads|
  type Bytes' (x :: Int) = Bytes <| max 0 x |>

  type PCAP = partition (PCAPHeader, [Packet]) using none

  data PCAPHeader = PCAPHeader {
      constrain pchMagicNum   :: Bits32 32 where <| pchMagicNum == 0xa1b2c3d4 |>,
      constrain pchVersionMaj :: Bits16 16 where <| pchVersionMaj == 2 |>,
      constrain pchVersionMin :: Bits16 16 where <| pchVersionMin == 4 |> ,
      constrain pchThisZone   :: Bits32 32 where <| pchThisZone == 0 |>,
      constrain pchSigFigs    :: Bits32 32 where <| pchSigFigs == 0 |>,
      pchSnapLen              :: Bits32 32,
      constrain pchNetwork    :: Bits32 32 where <| pchNetwork == 1 |>
  }

  type PacketN = partition Packet using none
  type Packets = partition [Packet] using none

  data Packet = Packet {
      --constrain tsSec   :: Bits32 32 where <| tsSec == (floor $ unsafePerformIO getPOSIXTime) |>,
      tsSec             :: Bits32 32,
      constrain tsUsec  :: Bits32 32 where <| tsUsec <= 999999999 |>,
                           Bits32 21,
      constrain inclLen :: Bits16 11 where <| inclLen >= 60 && inclLen <= 1514 |>,
                           Bits32 21,
      constrain origLen :: Bits16 11 where <| origLen == inclLen |>,
      body              :: Ethernet inclLen
  }

  data Ethernet (inclLen :: Bits16) = Ethernet {
    ethDst            :: MacAddr,
    ethSrc            :: MacAddr,
    constrain ethType :: Bits16 16 where <| ethType == 2048 |>,
    ethPayload        :: EthPayload <| (ethType, inclLen) |>
    --ethCRC            :: Bits32 32
  }

  data EthPayload (ethType :: Bits16, inclLen :: Bits16) = case ethType of
    2048 -> IPV4 {
      constrain ipv4Version  :: Bits8 4 where <| ipv4Version == 4 |>,
      constrain ipv4IHL      :: Bits8 4 where <| ipv4IHL == 5 |>,
      ipv4DSCP               :: Bits8 6,
      ipv4ECN                :: Bits8 2,
      constrain ipv4TotLen   :: Bits16 16 where <| ipv4TotLen == (max 0 $ (fromIntegral inclLen) - 14) |>,
      ipv4ID                 :: Bits16 16,
      ipv4Flags              :: IPV4Flags,
      constrain ipv4FragOff  :: Bits16 13 where <| ipv4FragOff == 0 |>,
      ipv4TTL                :: Bits8 8,
      constrain ipv4Protocol :: Bits8 8 where <| ipv4Protocol == 6 |>,
      ipv4Cksum              :: Bits16 16,
      ipv4Src                :: Bits32 32,
      ipv4Dst                :: Bits32 32,
      ipv4Opts               :: Bytes <| 4 * (max 0 $ (fI ipv4IHL) - 5) |>,
      ipv4Payload            :: IPV4Payload <| (ipv4Protocol, ipv4IHL, ipv4TotLen) |>
    }

  data IPV4Flags = IPV4Flags {
    constrain ipv4Res :: BitBool where <| ipv4Res == False |>,
    ipv4DF            :: BitBool,
    constrain ipv4MF  :: BitBool where <| ipv4MF == False |>
  }

  data IPV4Payload (prot :: Bits8, ipv4IHL :: Bits8, totLen :: Bits16) = TCP {
    tcpSrc                :: Bits16 16,
    constrain tcpDst      :: Bits16 16 where <| tcpDst == 80 |>,
    tcpSeq                :: Bits32 32,
    tcpAck                :: Bits32 32,
    constrain tcpOffset   :: Bits8 4 where <| tcpOffset == 5 |>,
    constrain tcpReserved :: Bits8 3 where <| tcpReserved == 0 |>,
    tcpFlags              :: TCPFlags,
    tcpWindow             :: Bits16 16,
    tcpCksum              :: Bits16 16,
    tcpUrgPtr             :: Bits16 16,
    tcpOptions            :: Bytes <| 4 * (max 0 $ (fI tcpOffset) - 5) |>,
    tcpPayload            :: Bytes' <| (fromIntegral totLen) - (fromIntegral $ (tcpOffset * 4) + (ipv4IHL * 4)) |>
  }

  data TCPFlags = TCPFlags {
    tcpNS  :: BitBool,
    tcpCWR :: BitBool,
    tcpECE :: BitBool,
    tcpURG :: BitBool,
    tcpACK :: BitBool,
    tcpPSH :: BitBool,
    constrain tcpRST :: BitBool where <| tcpRST == False |>,
    tcpSYN :: BitBool,
    tcpFIN :: BitBool
  }

  data MacAddr = MacAddr {
    constrain m1 :: Bits8 8 where <| m1 `mod` 2 == 0 |>,
    m2           :: Bits8 8,
    m3           :: Bits8 8,
    m4           :: Bits8 8,
    m5           :: Bits8 8,
    m6           :: Bits8 8
  }
|]

pCAPCycleTest_name = "PCAP Cycle"
pCAPCycleTest = do
  ps <- replicateM 5 (runGen pCAP_genM)
  let ps_serialized = map ((map word8ToChr) . fromChunks . fromCL . pCAP_serialize) ps
  let ps_parsed = map (fst . fst . pCAP_parseS) ps_serialized
  return $ ps == ps_parsed

writePCAP :: IO ()
writePCAP = do
  pcap <- runGen pCAP_genM
  B.writeFile "test2.pcap" $ (B.pack . fromChunks . fromCL . pCAP_serialize) pcap


-------------------------------------------------------------------------------
-- Unit testing of fromChunks function

emptyChunksTest_name = "Empty Chunks"
emptyChunksTest_expected = []
emptyChunksTest_got = fromChunks []
emptyChunksTest
  = TestCase (emptyChunksTest_expected @=? emptyChunksTest_got)

charChunksTest_name = "CharChunks"
charChunksTest_expected = [100,99,98]
charChunksTest_got = fromChunks [CharChunk 'd',CharChunk 'c',CharChunk 'b']
charChunksTest
  = TestCase (charChunksTest_expected @=? charChunksTest_got)

binaryChunksTest_name = "Binary Chunks"
binaryChunksTest_expected = [100, 100]
binaryChunksTest_got
  = fromChunks
      [(BinaryChunk 12) 5, (BinaryChunk 8) 4, (BinaryChunk 6) 3,
       (BinaryChunk 4) 4]
binaryChunksTest
  = TestCase (binaryChunksTest_expected @=? binaryChunksTest_got)

misalignedChunksTest_name = "Misaligned Chunks"
misalignedChunksTest_expected = [100, 96]
misalignedChunksTest_got
  = fromChunks
      [(BinaryChunk 12) 5, (BinaryChunk 8) 4, (BinaryChunk 6) 3]
misalignedChunksTest
  = TestCase
      (misalignedChunksTest_expected
         @=? misalignedChunksTest_got)

misalignedChunks2Test_name = "Misaligned Chunks 2"
misalignedChunks2Test_expected = [192]
misalignedChunks2Test_got = fromChunks [(BinaryChunk 3) 2]
misalignedChunks2Test
  = TestCase (misalignedChunks2Test_expected @=? misalignedChunks2Test_got)

mixedChunksTest_name = "Mixed Chunks"
mixedChunksTest_expected = [76, 128, 200]
mixedChunksTest_got
  = fromChunks
      [(BinaryChunk 2) 3, CharChunk 'd', (BinaryChunk 1) 6,
       (BinaryChunk 2) 2, (BinaryChunk 1) 2]
mixedChunksTest
  = TestCase (mixedChunksTest_expected @=? mixedChunksTest_got)


tests = TestList [ charTest_name              ~: charTest
                 , charCycleTest_name         ~: charCycleTest
                 , intTest_name               ~: intTest
                 , intCycleTest_name          ~: intCycleTest
                 , bits8Test_name             ~: bits8Test
                 , bits8MisalignedTest_name   ~: bits8MisalignedTest
                 , bits16Test_name            ~: bits16Test
                 , bits16CycleTest_name       ~: bits16CycleTest
                 , bits16MisalignedTest_name  ~: bits16MisalignedTest
                 , bits32Test_name            ~: bits32Test
                 , bits32CycleTest_name       ~: bits32CycleTest
                 , bits64Test_name            ~: bits64Test
                 , bits64CycleTest_name       ~: bits64CycleTest
                 , bitBoolTest_name           ~: bitBoolTest
                 , bitBoolCycleTest_name      ~: bitBoolCycleTest
                 , bitFieldCycleTest_name     ~: bitFieldCycleTest
                 , bytesTest_name             ~: bytesTest
                 , myStringCTest_name         ~: myStringCTest
                 , myStringCCycleTest_name    ~: myStringCCycleTest
                 , myTupleTest_name           ~: myTupleTest
                 , myTupleCycleTest_name      ~: myTupleCycleTest
                 , byteTest_name              ~: byteTest
                 , byteCycleTest_name         ~: byteCycleTest
                 , twoBytesTest_name          ~: twoBytesTest
                 , twoBytesCycleTest_name     ~: twoBytesCycleTest
                 , nestedTupleTest_name       ~: nestedTupleTest
                 , constrainedStringTest_name ~: constrainedStringTest
                 , constrainedGenTest_name    ~: constrainedGenTest
                 , regularListTest_name       ~: regularListTest
                 , sepListTest_name           ~: sepListTest
                 , sepTermListTest_name       ~: sepTermListTest
                 , sepTermListBytesTest_name  ~: sepTermListBytesTest
                 , pixelTest_name             ~: pixelTest
                 , pixelBytesTest_name        ~: pixelBytesTest
                 , pixelCycleTest_name        ~: pixelCycleTest
                 , recordConstantsTest_name   ~: recordConstantsTest
                 , constantsCycleTest_name    ~: constantsCycleTest
                 , fooFooTest_name            ~: fooFooTest
                 , fooBarTest_name            ~: fooBarTest
                 , myConstr1WithArgsTest_name ~: myConstr1WithArgsTest
                 , myConstr2NoArgsTest_name   ~: myConstr2NoArgsTest
                 , myConstr3TVArgsTest_name   ~: myConstr3TVArgsTest
                 , myConstr4NoArgsTest_name   ~: myConstr4NoArgsTest
                 --, myConstrCycleTest_name     ~: myConstrCycleTest
                 , myListEmptyTest_name       ~: myListEmptyTest
                 , myListNonemptyTest_name    ~: myListNonemptyTest
                 , myListCycleTest_name       ~: myListCycleTest
                 , nTTest_name                ~: nTTest
                 , nTCycleTest_name           ~: nTCycleTest
                 , switchTest_name            ~: switchTest
                 , switchCycleTest_name       ~: switchCycleTest
                 , dependentSerTest_name      ~: dependentSerTest
                 , dependentGenTest_name      ~: dependentGenTest
                 , paramSerTest_name          ~: paramSerTest
                 , paramGenTest_name          ~: paramGenTest
                 , hexObtainTest_name         ~: hexObtainTest
                 , hexCycleTest_name          ~: hexCycleTest
                 , littleInt8Test_name        ~: littleInt8Test
                 , littleInt16Test_name       ~: littleInt16Test
                 , littleInt32Test_name       ~: littleInt32Test
                 , bigInt8Test_name           ~: bigInt8Test
                 , bigInt16Test_name          ~: bigInt16Test
                 , bigInt32Test_name          ~: bigInt32Test
                 , pCAPCycleTest_name         ~: pCAPCycleTest
                 , emptyChunksTest_name       ~: emptyChunksTest
                 , charChunksTest_name        ~: charChunksTest
                 , binaryChunksTest_name      ~: binaryChunksTest
                 , misalignedChunksTest_name  ~: misalignedChunksTest
                 , misalignedChunks2Test_name ~: misalignedChunks2Test
                 , mixedChunksTest_name       ~: mixedChunksTest
                 ]

test = runTestTT tests

main :: IO ()
main = test >> return ()
