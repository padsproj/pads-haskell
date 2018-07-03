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

fromChunks :: [Chunk] -> IO B.ByteString
fromChunks cs = do
    let bits = foldr getBits 0 cs
    i <- combineChunks cs bits
    w8s <- reverse <$> createWord8s i
    -- return guard: when >=8 leading zeroes are randomly generated,
    -- createWord8s behaves improperly by stripping them - add them back in here
    return $ if   length w8s /= bits `div` 8
             then B.pack $ (replicate ((bits `div` 8) - length w8s) 0) ++ w8s
             else B.pack w8s

    where
        getBits :: Chunk -> Int -> Int
        getBits (CharChunk _)     z = 8 + z
        getBits (BinaryChunk _ b) z = b + z

        combineChunks :: [Chunk] -> Int -> IO Integer
        combineChunks [] _ = return 0
        combineChunks cs 0 = case cs of [BinaryChunk _ 0] -> return 0
                                        _ -> error $ "ran out of bits" ++ show cs
        combineChunks ((CharChunk c):cs) bs = do
            let i = ((fromIntegral . chrToWord8) c) `shiftL` (bs - 8)
            rest <- combineChunks cs (bs - 8)
            return $ i + rest
        combineChunks ((BinaryChunk v b):cs) bs = do
            let i = (fromIntegral $ v .&. (2^b - 1)) `shiftL` (bs - b)
            rest <- combineChunks cs (bs - b)
            return $ i + rest

        -- Outcome will need to be reversed
        createWord8s :: Integer -> IO [Word8]
        createWord8s 0 = return []
        createWord8s i = do
            let w = fromIntegral $ i .&. 255
            rest <- createWord8s (i `shiftR` 8)
            return (w:rest)

charTest_name = "Char"
charTest_expected = [CharChunk 'a']
charTest_got = fromCL $ char_serialize 'a'
charTest = TestCase (charTest_expected @=? charTest_got)

intTest_name = "Int"
intTest_expected = [CharChunk '2']
intTest_got = fromCL $ int_serialize (2 :: Int)
intTest = TestCase (intTest_expected @=? intTest_got)


[pads| type MyTuple = (Int,'c',Bits8 4,'d') |]

myTupleTest_name = "MyTuple"
myTupleTest_expected
  = [CharChunk '1', CharChunk 'c', (BinaryChunk 10) 4, CharChunk 'd']
myTupleTest_got = fromCL $ myTuple_serialize ((1, 10) :: MyTuple)
myTupleTest
  = TestCase (myTupleTest_expected @=? myTupleTest_got)


[pads| type Byte = Bits8 8 |]

byteTest_name = "Byte"
byteTest_expected = [(BinaryChunk 254) 8]
byteTest_got = fromCL $ byte_serialize (254 :: Byte)
byteTest = TestCase (byteTest_expected @=? byteTest_got)


[pads| type JustAChar = Char |]

justACharTest_name = "JustAChar"
justACharTest_expected = [CharChunk 'a']
justACharTest_got = fromCL $ justAChar_serialize 'a'
justACharTest
  = TestCase (justACharTest_expected @=? justACharTest_got)


[pads| type TwoBytes = (Byte, Byte) |]

twoBytesTest_name = "TwoBytes"
twoBytesTest_expected = [(BinaryChunk 0) 8, (BinaryChunk 122) 8]
twoBytesTest_got = fromCL $ twoBytes_serialize ((0, 122) :: TwoBytes)
twoBytesTest
  = TestCase (twoBytesTest_expected @=? twoBytesTest_got)


[pads| type RegularList = [Bits8 8]
       type SepList     = [Bits8 8 | '|']
       type SepTermList = [Bits8 8 | '|'] terminator Char
       --type SepTermList2 = [Bits8 8 | '|'] terminator EOR |]

regularListTest_name = "RegularList"
regularListTest_expected = [(BinaryChunk 10) 8, (BinaryChunk 5) 8]
regularListTest_got
  = fromCL $ regularList_serialize ([10, 5] :: RegularList)
regularListTest
  = TestCase (regularListTest_expected @=? regularListTest_got)

sepListTest_name = "SepList"
sepListTest_expected
  = [(BinaryChunk 10) 8, CharChunk '|', (BinaryChunk 5) 8]
sepListTest_got = fromCL $ sepList_serialize ([10, 5] :: SepList)
sepListTest
  = TestCase (sepListTest_expected @=? sepListTest_got)

sepTermListTest_name = "SepTermList"
sepTermListTest_expected
  = [(BinaryChunk 10) 8, CharChunk '|', (BinaryChunk 5) 8, CharChunk 'X']
sepTermListTest_got
  = fromCL $ sepTermList_serialize ([10, 5] :: SepTermList)
sepTermListTest
  = TestCase (sepTermListTest_expected @=? sepTermListTest_got)

sepTermListBytesTest_name = "SepTermList Bytes"
sepTermListBytesTest_expected = B.pack [10, 124, 5, 88]
sepTermListBytesTest_got
  = (unsafePerformIO $ (fromChunks $ sepTermListTest_got))
sepTermListBytesTest
  = TestCase (sepTermListBytesTest_expected @=? sepTermListBytesTest_got)


[pads| data Pixel = Pixel { a :: Bits16 9
                          , b :: Bits8 5
                          , c :: Bits8 5
                          , d :: Bits8 5
                          , pb_index :: Bits8 4
                          , pr_index :: Bits8 4 } deriving Generic |]

pixelTest_name = "Pixel"
pixelTest_expected
  = [(BinaryChunk 511) 9, (BinaryChunk 0) 5, (BinaryChunk 8) 5,
     (BinaryChunk 0) 5, (BinaryChunk 1) 4, (BinaryChunk 1) 4]
pixelTest_got = fromCL $ pixel_serialize ((((((Pixel 511) 0) 8) 0) 1) 1)
pixelTest
  = TestCase (pixelTest_expected @=? pixelTest_got)

pixelBytesTest_name = "Pixel Bytes"
pixelBytesTest_expected = B.pack [255, 129, 0, 17]
pixelBytesTest_got
  = (unsafePerformIO $ (fromChunks $ (pixelTest_got)))
pixelBytesTest
  = TestCase (pixelBytesTest_expected @=? pixelBytesTest_got)

mkGTest "Pixel Gen" ["Pixel"]


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

mkGTest "Foo Gen" ["Foo","Bar"]


[pads| data MyConstr = MyConstr1 Int Char
                     | MyConstr2 Void |]

myConstrWithArgsTest_name = "MyConstr With Args"
myConstrWithArgsTest_expected
  = [CharChunk '1', CharChunk '2', CharChunk 'x']
myConstrWithArgsTest_got
  = (fromCL $ (myConstr_serialize ((MyConstr1 12) 'x')))
myConstrWithArgsTest
  = TestCase (myConstrWithArgsTest_expected @=? myConstrWithArgsTest_got)

myConstrNoArgsTest_name = "MyConstr No Args"
myConstrNoArgsTest_expected = []
myConstrNoArgsTest_got
  = (fromCL $ (myConstr_serialize MyConstr2))
myConstrNoArgsTest
  = TestCase (myConstrNoArgsTest_expected @=? myConstrNoArgsTest_got)

myConstrGenTest_name = "MyConstr Gen"
myConstr_check MyConstr1 {} = True
myConstr_check MyConstr2 {} = True
myConstrGenTest
  = TestCase
      ((assertBool "MyConstr Gen")
         (myConstr_check $ (unsafePerformIO myConstr_genM)))


[pads| data MyList a = MyCons a (MyList a)
                     | MyNil Void |]

myListEmptyTest_name = "MyList Empty"
myListEmptyTest_expected = []
myListEmptyTest_got
  = (fromCL $ ((myList_serialize char_serialize) MyNil))
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


tests = TestList [ TestLabel charTest_name charTest
                 , TestLabel intTest_name intTest
                 , TestLabel myTupleTest_name myTupleTest
                 , TestLabel byteTest_name byteTest
                 , TestLabel justACharTest_name justACharTest
                 , TestLabel twoBytesTest_name twoBytesTest
                 , TestLabel regularListTest_name regularListTest
                 , TestLabel sepListTest_name sepListTest
                 , TestLabel sepTermListTest_name sepTermListTest
                 , TestLabel sepTermListBytesTest_name sepTermListBytesTest
                 , TestLabel pixelTest_name pixelTest
                 , TestLabel pixelBytesTest_name pixelBytesTest
                 , TestLabel pixelGenTest_name pixelGenTest
                 , TestLabel fooFooTest_name fooFooTest
                 , TestLabel fooBarTest_name fooBarTest
                 , TestLabel fooGenTest_name fooGenTest
                 , TestLabel myConstrNoArgsTest_name myConstrNoArgsTest
                 , TestLabel myConstrWithArgsTest_name myConstrWithArgsTest
                 , TestLabel myListEmptyTest_name myListEmptyTest
                 , TestLabel myListNonemptyTest_name myListNonemptyTest
                 ]

test = runTestTT tests

[pads| type MyString = constrain s :: StringFW 100 where <| take 3 s == "ccc" |> |]




-- pixelTestName = "Pixel composition"
-- pixelCheck :: Pixel -> Bool
-- pixelCheck (Pixel a b c d pb pr) = assertBetween a  0 (2^9-1)
--                                 && assertBetween b  0 (2^5-1)
--                                 && assertBetween c  0 (2^5-1)
--                                 && assertBetween d  0 (2^5-1)
--                                 && assertBetween pb 0 (2^4-1)
--                                 && assertBetween pr 0 (2^4-1)
--
-- pixelTest = do
--   pixels <- sequence $ replicate 1000 pixel_genM
--   return $ (pixels, all (== True) $ map pixelCheck pixels)





-- data Test = Test String Bool
--     deriving Show
--
-- makeMany :: String -> IO [[Char]]
-- makeMany s = replicateM replicateVal $ generate s
--     where
--         replicateVal = 500
--
-- assertLengths :: (Int -> Bool) -> [[a]] -> Bool
-- assertLengths p xss = all p $ map length xss

-- createTest 1 "SomeBytes length"  [| assertLengths (== 8) samples |] Nothing
-- createTest 2 "Pixel length"      [| assertLengths (== 4) samples |] Nothing
-- createTest 3 "Pixel composition" [| all (\x -> x == "" || Prelude.head x == '\n') $ (map (snd . pixel_parseS)) samples |] Nothing
-- createTest 4 "Pixel parse"       [| all (== 0) $ map (numErrors . fst . snd . fst . pixelP_parseS) samples |] Nothing
-- createTest 5 "Mixed length"      [| assertLengths (== 2) samples |] Nothing
-- createTest 6 "Mixed composition" [| and $ map checkChar samples |] $
--     Just [d| checkChar xs = let
--                 x0 = chrToWord8 $ xs !! 0
--                 x1 = chrToWord8 $ xs !! 1
--                 in (x0 `shiftL` 4) + (x1 `shiftR` 4) == chrToWord8 'c' |]
-- createTest 7 "Dependent length" [| assertLengths (\x -> x >= 0 && x <= 3) samples |] Nothing
-- createTest 8 "Q1 composition"   [| all (== 0) $ map (snd . checkSubstrings) samples |] $
--     Just [d| checkSubstrings [] = (undefined, 0)
--              checkSubstrings xs = foldr comp (last xs, 0) (init xs)
--
--              comp x (y, z) =
--                  case (x, y) of ('1', '0') -> (x, z + 1)
--                                 ('0', '1') -> (x, z - 1)
--                                 _          -> (x, z) |]

-- test1 = do
--     let name = "SomeBytes length"
--     printf "Running test %s\n" name
--     samples <- makeMany "SomeBytes"
--     let result = assertLengths (== 8) samples
--     return $ Test name result
-- test2 = do
--     let name = "Pixel length"
--     printf "Running test %s\n" name
--     samples <- makeMany "Pixel"
--     let result = assertLengths (== 4) samples
--     return $ Test name result
-- test3 = do
--     let name = "Pixel composition"
--     printf "Running test %s\n" name
--     samples <- makeMany "Pixel"
--     let result = all (\x -> x == "" || Prelude.head x == '\n') $ (map (snd . pixel_parseS)) samples
--     return $ Test name result
-- test4 = do
--     let name = "Pixel parse"
--     printf "Running test %s\n" name
--     samples <- makeMany "Pixels"
--     let result = all (== 0) $ map (numErrors . fst . snd . fst . pixels_parseS) samples
--     return $ Test name result
-- test5 = do
--     let name = "Mixed length"
--     printf "Running test %s\n" name
--     samples <- makeMany "Mixed"
--     let result = assertLengths (== 2) samples
--     return $ Test name result
-- test6 = do
--     let name = "Mixed composition"
--     printf "Running test %s\n" name
--     samples <- makeMany "Mixed"
--     let result = and $ map checkChar samples
--     return $ Test name result
--     where
--         checkChar xs = let
--             x0 = chrToWord8 $ xs !! 0
--             x1 = chrToWord8 $ xs !! 1
--             in (x0 `shiftL` 4) + (x1 `shiftR` 4) == chrToWord8 'c'
-- test7 = do
--     let name = "Dependent length"
--     printf "Running test %s\n" name
--     samples <- makeMany "Dependent"
--     let result = assertLengths (\x -> x >= 0 && x <= 3) samples
--     return $ Test name result
-- test8 = do
--     let name = "Q1 composition"
--     printf "Running test %s\n" name
--     samples <- makeMany "Q1"
--     let result = all (== 0) $ map (snd . checkSubstrings) samples
--     return $ Test name result
--     where
--         checkSubstrings :: [Char] -> (Char, Int)
--         checkSubstrings [] = (undefined, 0)
--         checkSubstrings xs = foldr comp (last xs, 0) (init xs)
--
--         comp :: Char -> (Char, Int) -> (Char, Int)
--         comp x (y, z) =
--             case (x, y) of ('1', '0') -> (x, z + 1)
--                            ('0', '1') -> (x, z - 1)
--                            _          -> (x, z)
