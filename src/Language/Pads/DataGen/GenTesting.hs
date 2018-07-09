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

module Language.Pads.DataGen.GenTesting where

import           Control.Monad
import           Data.Bits
import qualified Data.ByteString as B
import           Data.Word
import           GHC.Generics
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           System.IO.Unsafe (unsafePerformIO)
import           Test.HUnit hiding (test)

import Language.Pads.Padsc
import Language.Pads.DataGen.GenTestingTH

-- fromChunks :: [Chunk] -> IO B.ByteString
-- fromChunks cs = do
--     let bits = foldr getBits 0 cs
--     i <- combineChunks cs bits
--     w8s <- reverse <$> createWord8s i
--     -- return guard: when >=8 leading zeroes are randomly generated,
--     -- createWord8s behaves improperly by stripping them - add them back in here
--     return $ if   length w8s /= bits `div` 8
--              then B.pack $ (replicate ((bits `div` 8) - length w8s) 0) ++ w8s
--              else B.pack w8s
--
--     where
--         getBits :: Chunk -> Int -> Int
--         getBits (CharChunk _)     z = 8 + z
--         getBits (BinaryChunk _ b) z = b + z
--
--         combineChunks :: [Chunk] -> Int -> IO Integer
--         combineChunks [] _ = return 0
--         combineChunks cs 0 = case cs of [BinaryChunk _ 0] -> return 0
--                                         _ -> error $ "ran out of bits" ++ show cs
--         combineChunks ((CharChunk c):cs) bs = do
--             let i = ((fromIntegral . chrToWord8) c) `shiftL` (bs - 8)
--             rest <- combineChunks cs (bs - 8)
--             return $ i + rest
--         combineChunks ((BinaryChunk v b):cs) bs = do
--             let i = (fromIntegral $ v .&. (2^b - 1)) `shiftL` (bs - b)
--             rest <- combineChunks cs (bs - b)
--             return $ i + rest
--
--         -- Outcome will need to be reversed
--         createWord8s :: Integer -> IO [Word8]
--         createWord8s 0 = return []
--         createWord8s i = do
--             let w = fromIntegral $ i .&. 255
--             rest <- createWord8s (i `shiftR` 8)
--             return (w:rest)

sampleSize = 100 -- used for "cycle" testing - generate, serialize, parse

--------------------------------------------------------------------------------
-- Unit testing of serialization of pure/primitive PADS types

charTest_name = "Char"
charTest_expected = [chrToWord8 'a']
charTest_got = fromChunks' $ fromCL $ char_serialize 'a'
charTest = TestCase (charTest_expected @=? charTest_got)

charCycleTest_name = "Char Cycle"
charCycleTest = do
  cs <- replicateM sampleSize char_genM
  let cs_serialized = map ((map word8ToChr) . fromChunks' . fromCL . char_serialize) cs
  let cs_parsed = map (fst . fst . (parseStringInput char_parseM)) cs_serialized
  return $ all (== True) (map (\(x,y) -> x == y) (zip cs cs_parsed))

intTest_name = "Int"
intTest_expected = [chrToWord8 '2']
intTest_got = fromChunks' $ fromCL $ int_serialize 2
intTest = TestCase (intTest_expected @=? intTest_got)

intCycleTest_name = "Int Cycle"
intCycleTest = do
  xs <- replicateM sampleSize int_genM
  let xs_serialized = map ((map word8ToChr) . fromChunks' . fromCL . int_serialize) xs
  let xs_parsed = map (fst . fst . (parseStringInput int_parseM)) xs_serialized
  return $ all (== True) (map (\(x,y) -> x == y) (zip xs xs_parsed))

bits8Test_name = "Bits8"
bits8Test_expected = [98]
bits8Test_got = (fromChunks' $ (fromCL $ ((bits8_serialize 8) 98)))
bits8Test = TestCase (bits8Test_expected @=? bits8Test_got)

bits8CycleTest_name = "Bits8 Cycle"
bits8CycleTest = do
  xs <- (filter (/= chrToWord8 '\n')) <$> replicateM sampleSize (bits8_genM 8)
  let xs_serialized = map ((map word8ToChr) . fromChunks' . fromCL . (bits8_serialize 8)) xs
  let xs_parsed = map (fst . fst . (parseStringInput (bits8_parseM 8))) xs_serialized
  return $ all (== True) (map (\(x,y) -> x == y) (zip xs xs_parsed))

bits8MisalignedTest_name = "Bits8 Misaligned"
bits8MisalignedTest_expected = [224]
bits8MisalignedTest_got
  = (fromChunks' $ (fromCL $ ((bits8_serialize 3) 7)))
bits8MisalignedTest
  = TestCase
      (bits8MisalignedTest_expected @=? bits8MisalignedTest_got)

-- | Use BitField generator and serializer but BitField50 parser to avoid record
-- discipline problems
[pads| type BitField50 = partition BitField 50 using none |]
bitFieldCycleTest_name = "BitField Cycle"
bitFieldCycleTest = do
  xs <- replicateM sampleSize (bitField_genM 50)
  let xs_serialized = map ((map word8ToChr) . fromChunks' . fromCL . (bitField_serialize 50)) xs
  let xs_parsed = map (fst . fst . bitField50_parseS) xs_serialized
  return $ all (== True) (map (\(x,y) -> x == y) (zip xs xs_parsed))

bytesTest_name = "Bytes"
bytesTest_expected = [1, 2, 3, 4]
bytesTest_got
  = (fromChunks' $ (fromCL $ ((bytes_serialize 4) $ (B.pack [1, 2, 3, 4]))))
bytesTest = TestCase (bytesTest_expected @=? bytesTest_got)

[pads| type MyStringC = StringC 'f' |]
--mkSTest "MyStringC" [CharChunk 't',CharChunk 'g',CharChunk 'i',CharChunk 'f'] [| fromCL $ myStringC_serialize "tgi" |]
myStringCTest_name = "MyStringC"
myStringCTest_expected
  = [CharChunk 't', CharChunk 'g', CharChunk 'i', CharChunk 'f']
myStringCTest_got = (fromCL $ (myStringC_serialize "tgi"))
myStringCTest
  = TestCase (myStringCTest_expected @=? myStringCTest_got)

myStringCCycleTest_name = "MyStringC Cycle"
myStringCCycleTest = do
  ss <- replicateM sampleSize myStringC_genM
  let ss_serialized = map ((map word8ToChr) . fromChunks' . fromCL . myStringC_serialize) ss
  let ss_parsed = map (fst . fst . myStringC_parseS) ss_serialized
  return $ all (== True) (map (\(x,y) -> x == y) (zip ss ss_parsed))

-------------------------------------------------------------------------------
-- Unit testing of user-defined PADS types

[pads| type MyTuple = (Int,'c',Bits8 4,'d') |]

myTupleTest_name = "MyTuple"
myTupleTest_expected
  = [CharChunk '1', CharChunk 'c', (BinaryChunk 10) 4, CharChunk 'd']
myTupleTest_got = fromCL $ myTuple_serialize (1, 10)
myTupleTest
  = TestCase (myTupleTest_expected @=? myTupleTest_got)

myTupleCycleTest_name = "MyTuple Cycle"
myTupleCycleTest = do
  ts <- replicateM sampleSize myTuple_genM
  let ts_serialized = map ((map word8ToChr) . fromChunks' . fromCL . myTuple_serialize) ts
  let ts_parsed = map (fst . fst . myTuple_parseS) ts_serialized
  return $ all (== True) (map (\(x,y) -> x == y) (zip ts ts_parsed))


[pads| type Byte = Bits8 8 |]

byteTest_name = "Byte"
byteTest_expected = [(BinaryChunk 254) 8]
byteTest_got = fromCL $ byte_serialize (254 :: Byte)
byteTest = TestCase (byteTest_expected @=? byteTest_got)

byteCycleTest_name = "Byte Cycle"
byteCycleTest = do
  ts <- (filter (/= (chrToWord8 '\n'))) <$> replicateM sampleSize byte_genM
  let ts_serialized = map ((map word8ToChr) . fromChunks' . fromCL . byte_serialize) ts
  let ts_parsed = map (fst . fst . byte_parseS) ts_serialized
  return $ all (== True) (map (\(x,y) -> x == y) (zip ts ts_parsed))


[pads| type TwoBytes = (Byte, Byte)
       type TwoBytesP = partition TwoBytes using none |] -- TODO

twoBytesTest_name = "TwoBytes"
twoBytesTest_expected = [(BinaryChunk 0) 8, (BinaryChunk 122) 8]
twoBytesTest_got = fromCL $ twoBytes_serialize ((0, 122) :: TwoBytes)
twoBytesTest
  = TestCase (twoBytesTest_expected @=? twoBytesTest_got)

twoBytesCycleTest_name = "TwoBytes Cycle"
twoBytesCycleTest = do
  ts <- replicateM sampleSize twoBytes_genM
  let ts_serialized = map ((map word8ToChr) . fromChunks' . fromCL . twoBytes_serialize) ts
  let ts_parsed = map (fst . fst . twoBytesP_parseS) ts_serialized
  return $ all (== True) (map (\(x,y) -> x == y) (zip ts ts_parsed))

-- Nested tuple
[pads| type TupleN = (Int, ',', (Int,':',Int), ';', Int) |]
nestedTupleTest_name = "Nested Tuple"
nestedTupleTest_expected
  = [CharChunk '1', CharChunk ',', CharChunk '2', CharChunk ':',
     CharChunk '3', CharChunk ';', CharChunk '4']
nestedTupleTest_got = fromCL $ tupleN_serialize (1, (2, 3), 4)
nestedTupleTest
  = TestCase (nestedTupleTest_expected @=? nestedTupleTest_got)

-- PLists of several forms
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
sepTermListBytesTest_got = fromChunks' sepTermListTest_got
sepTermListBytesTest
  = TestCase (sepTermListBytesTest_expected @=? sepTermListBytesTest_got)

-- Run-of-the-mill record with sub-byte data
[pads| data Pixel = Pixel { a :: Bits16 9
                          , b :: Bits8 5
                          , c :: Bits8 5
                          , d :: Bits8 5
                          , pb_index :: Bits8 4
                          , pr_index :: Bits8 4 }

       type PixelNone = partition Pixel using none |]

pixelTest_name = "Pixel"
pixelTest_expected
  = [(BinaryChunk 511) 9, (BinaryChunk 0) 5, (BinaryChunk 8) 5,
     (BinaryChunk 0) 5, (BinaryChunk 1) 4, (BinaryChunk 1) 4]
pixelTest_got = fromCL $ pixel_serialize ((((((Pixel 511) 0) 8) 0) 1) 1)
pixelTest
  = TestCase (pixelTest_expected @=? pixelTest_got)

pixelBytesTest_name = "Pixel Bytes"
pixelBytesTest_expected = [255, 129, 0, 17]
pixelBytesTest_got = fromChunks' pixelTest_got
pixelBytesTest
  = TestCase (pixelBytesTest_expected @=? pixelBytesTest_got)

pixelCycleTest_name = "Pixel Cycle"
pixelCycleTest = do
  ps <- replicateM sampleSize pixel_genM
  let ps_serialized = map ((map word8ToChr) . fromChunks' . fromCL . pixel_serialize) ps
  let ps_parsed = map (fst . fst . pixelNone_parseS) ps_serialized
  return $ all (\(x,y) -> x == y) (zip ps ps_parsed)

-- Constants in records
[pads| data Constants = Constants { var1 :: Int
                                  , "string"
                                  , 'c'
                                  , var2 :: StringFW 10 } |]

recordConstantsTest_name = "Record Constants"
recordConstantsTest_expected
  = [52, 50, 115, 116, 114, 105, 110, 103, 99, 104, 101, 108, 108,
     111, 116, 104, 101, 114, 101] -- represents "42stringchellothere"
recordConstantsTest_got
  = (fromChunks'
       $ (fromCL $ (constants_serialize $ ((Constants 42) "hellothere"))))
recordConstantsTest
  = TestCase
      (recordConstantsTest_expected @=? recordConstantsTest_got)

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
[pads| data MyConstr = MyConstr1 Int Char
                     | MyConstr2 Void
                     | MyConstr3 "Hello" |]

myConstr1WithArgsTest_name = "MyConstr1 With Args"
myConstr1WithArgsTest_expected
  = [CharChunk '1', CharChunk '2', CharChunk 'x']
myConstr1WithArgsTest_got
  = (fromCL $ (myConstr_serialize ((MyConstr1 12) 'x')))
myConstr1WithArgsTest
  = TestCase (myConstr1WithArgsTest_expected @=? myConstr1WithArgsTest_got)

myConstr2NoArgsTest_name = "MyConstr2 No Args"
myConstr2NoArgsTest_expected = []
myConstr2NoArgsTest_got
  = (fromCL $ (myConstr_serialize MyConstr2))
myConstr2NoArgsTest
  = TestCase (myConstr2NoArgsTest_expected @=? myConstr2NoArgsTest_got)

myConstr3NoArgsTest_name = "MyConstr3 No Args"
myConstr3NoArgsTest_expected = []
myConstr3NoArgsTest_got = (fromCL $ (myConstr_serialize MyConstr3))
myConstr3NoArgsTest
  = TestCase
      (myConstr3NoArgsTest_expected @=? myConstr3NoArgsTest_got)

myConstrGenTest_name = "MyConstr Gen"
myConstr_check MyConstr1 {} = True
myConstr_check MyConstr2 {} = True
myConstrGenTest
  = TestCase
      ((assertBool "MyConstr Gen")
         (myConstr_check $ (unsafePerformIO myConstr_genM)))

-- Test use of type variables and newtype
[pads| data MyList a = MyCons a (MyList a)
                     | MyNil Void

       newtype IntList = MyList Int |]

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

-- intListEmptyTest_name = "IntList Empty"
-- intListEmptyTest_expected = []
-- intListEmptyTest_got = (fromCL $ (intList_serialize MyNil))
-- intListEmptyTest
--   = TestCase (intListEmptyTest_expected @=? intListEmptyTest_got)
--
-- intListNonemptyTest_name = "IntList Nonempty"
-- intListNonemptyTest_expected
--   = [CharChunk '1', CharChunk '2', CharChunk '3']
-- intListNonemptyTest_got
--   = (fromCL
--        $ (intList_serialize ((MyCons 1) ((MyCons 2) ((MyCons 3) MyNil)))))
-- intListNonemptyTest
--   = TestCase
--       (intListNonemptyTest_expected @=? intListNonemptyTest_got)

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
  deps <- replicateM sampleSize dependent_genM
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
  params <- replicateM sampleSize param1_genM
  return $ all (== True)
    (map (\par -> (fromIntegral $ p1 par) == (B.length (p3 (p2 par)))) params)
paramGenTest
  = TestCase (assert paramGenTest_invariant)



-------------------------------------------------------------------------------
-- Unit testing of fromChunks function

emptyChunksTest_name = "Empty Chunks"
emptyChunksTest_expected = []
emptyChunksTest_got = fromChunks' []
emptyChunksTest
  = TestCase (emptyChunksTest_expected @=? emptyChunksTest_got)

charChunksTest_name = "CharChunks"
charChunksTest_expected = [100,99,98]
charChunksTest_got = fromChunks' [CharChunk 'd',CharChunk 'c',CharChunk 'b']
charChunksTest
  = TestCase (charChunksTest_expected @=? charChunksTest_got)

binaryChunksTest_name = "Binary Chunks"
binaryChunksTest_expected = [100, 100]
binaryChunksTest_got
  = fromChunks'
      [(BinaryChunk 12) 5, (BinaryChunk 8) 4, (BinaryChunk 6) 3,
       (BinaryChunk 4) 4]
binaryChunksTest
  = TestCase (binaryChunksTest_expected @=? binaryChunksTest_got)

misalignedChunksTest_name = "Misaligned Chunks"
misalignedChunksTest_expected = [100, 96]
misalignedChunksTest_got
  = fromChunks'
      [(BinaryChunk 12) 5, (BinaryChunk 8) 4, (BinaryChunk 6) 3]
misalignedChunksTest
  = TestCase
      (misalignedChunksTest_expected
         @=? misalignedChunksTest_got)

misalignedChunks2Test_name = "Misaligned Chunks 2"
misalignedChunks2Test_expected = [192]
misalignedChunks2Test_got = fromChunks' [(BinaryChunk 3) 2]
misalignedChunks2Test
  = TestCase (misalignedChunks2Test_expected @=? misalignedChunks2Test_got)

mixedChunksTest_name = "Mixed Chunks"
mixedChunksTest_expected = [76, 128, 200]
mixedChunksTest_got
  = fromChunks'
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
                 , regularListTest_name       ~: regularListTest
                 , sepListTest_name           ~: sepListTest
                 , sepTermListTest_name       ~: sepTermListTest
                 , sepTermListBytesTest_name  ~: sepTermListBytesTest
                 , pixelTest_name             ~: pixelTest
                 , pixelBytesTest_name        ~: pixelBytesTest
                 , pixelCycleTest_name        ~: pixelCycleTest
                 , recordConstantsTest_name   ~: recordConstantsTest
                 , fooFooTest_name            ~: fooFooTest
                 , fooBarTest_name            ~: fooBarTest
                 , myConstr1WithArgsTest_name ~: myConstr1WithArgsTest
                 , myConstr2NoArgsTest_name   ~: myConstr2NoArgsTest
                 , myConstr3NoArgsTest_name   ~: myConstr3NoArgsTest
                 , myListEmptyTest_name       ~: myListEmptyTest
                 , myListNonemptyTest_name    ~: myListNonemptyTest
                 --, intListEmptyTest_name      ~: intListEmptyTest
                 --, intListNonemptyTest_name   ~: intListNonemptyTest
                 , dependentSerTest_name      ~: dependentSerTest
                 , dependentGenTest_name      ~: dependentGenTest
                 , paramSerTest_name          ~: paramSerTest
                 , paramGenTest_name          ~: paramGenTest
                 , emptyChunksTest_name       ~: emptyChunksTest
                 , charChunksTest_name        ~: charChunksTest
                 , binaryChunksTest_name      ~: binaryChunksTest
                 , misalignedChunksTest_name  ~: misalignedChunksTest
                 , misalignedChunks2Test_name ~: misalignedChunks2Test
                 , mixedChunksTest_name       ~: mixedChunksTest

                 ]

test = runTestTT tests
