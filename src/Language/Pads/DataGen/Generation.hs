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
           , NamedFieldPuns #-}

module Language.Pads.DataGen.Generation where

import           Control.Monad
import           Data.Bits
import qualified Data.ByteString as B
import qualified Data.List as L
import           Data.Maybe
import           Data.Word
import qualified Language.Haskell.TH as TH
import qualified System.Random.MWC as MWC

import           Language.Pads.Padsc
import qualified Language.Pads.DataGen.Rand as RN
import           Language.Pads.DataGen.GenDescriptions

-- Bring into scope our pads ASTs
$(make_pads_declarations $ map snd padsSamples)
envs = padsSamples

gen_ast :: PadsDecl -> IO [GenType]
gen_ast (PadsDeclData id xs args padsdata e)   = gen_padsdatas padsdata      --
gen_ast (PadsDeclType id xs args padsty)       = (:[]) <$> genPadsTy padsty   -- Is any of this complete? What do the other args mean?
--gen_ast (PadsDeclNew  id xs args branchinfo e) = gen_branches branchinfo     --
--gen_ast (PadsDeclObtain id xs padsty e) =
gen_ast x = error $ "Error in gen_ast:  " ++ (show x)


-- gen_padsdatas :: PadsData -> IO [GenType]
-- gen_padsdatas (PUnion branches) = do
--     print "here"
--     bs <- mapM gen_branches branches --generate for each branch
--     let gs = map GTList bs               --make each branch a list
--     print "here"
--     return [Choice gs] --embed in a Choice type
-- -- gen_padsdatas (PSwitch e branches) =
-- gen_padsdatas x = error $ "Error in gen_padsdatas: " ++ (show x)

gen_padsdatas (PUnion branches) = do
    gen <- MWC.createSystemRandom
    b <- RN.randElem branches gen
    --bs <- mapM gen_branches branches --generate for each branch
    bs <- gen_branches b
    let gs = GTList bs               --make each branch a list
    print "here"
    return [gs] --[Choice gs] --embed in a CHOICE type
gen_padsdatas x = error $ "Error in gen_padsdatas: " ++ (show x)


gen_branches (BRecord id fs exp) =  mapM gen_fieldinfo fs
gen_branches (BConstr id fs e)   =  mapM gen_constarg fs


gen_fieldinfo ((id, constarg, exp)) = gen_constarg constarg -- Can't always disregard exp

gen_constarg  ((strict, padsty)) =  genPadsTy padsty

listLimit = 20 -- Limit how long generated lists can be.


-- This function actually takes apart padsty's
genPadsTy :: PadsTy -> IO GenType
genPadsTy (PTycon xs)            = gen_base $ xs !! 0
genPadsTy (PExpression exp)      = return $ gen_lit exp
genPadsTy (PApp xs (Just e))     = do
    pt <- genPadsTy $ xs !! 0
    return $ App pt (gen_lit e)
genPadsTy (PList pty delim term) = do
    t   <- genPadsTy pty
    gen <- MWC.createSystemRandom
    n   <- RN.randInt 1 listLimit gen
    case delim
      of Just d -> do
            d' <- genPadsTy d
            return $ GTList (L.intersperse d' (replicate n t))
         Nothing -> return $ GTList (replicate n t)
genPadsTy x = error $ "Error in genPadsTy: " ++ show x


-- step out into template haskell to get the literal value we need
-- and turn it into a GenType
gen_lit (TH.LitE (TH.CharL x))    = (Lit x)
gen_lit (TH.LitE (TH.StringL xs)) = (GTList (map Lit xs) ) -- treat string as a list of chars
gen_lit (TH.LitE (TH.IntegerL x)) = (Int (fromIntegral x))



data GenType = GTBitField -- Parameterized
             | GTBits8    -- Parameterized
             | GTBits16   -- Parameterized
             | GTBits32   -- Parameterized
             | GTBits64   -- Parameterized
             | GTStringC  -- Parameterized
             | GTStringFW -- Parameterized
             | GTChar
             | GTInt
             | GTList [ GenType ]
             | Lit Char    -- Argument to App
             | Int Int     -- Argument to App
             | App GenType GenType
             | Choice [ GenType ]
  deriving (Show)

gen_base "BitField"  = return GTBitField
gen_base "Bits8"     = return GTBits8
gen_base "Bits16"    = return GTBits16
gen_base "Bits32"    = return GTBits32
gen_base "Bits64"    = return GTBits64
gen_base "Char"      = return GTChar
gen_base "Int"       = return GTInt
gen_base "StringFW"  = return GTStringFW
gen_base "StringC"   = return GTStringC
-- this will be replaced with something which actually can find all the ASTs
-- in a single location
gen_base x = (gen_lookup x envs)

-- If we couldn't find a base type, maybe it's another PADS type...
-- try looking it up.
gen_lookup s ( (s1,b):xs ) | s == s1 = GTList <$> gen_ast b
gen_lookup s ( (x,b):xs ) = gen_lookup s xs
gen_lookup s [] = error ("could not find ast_" ++ s)


possLetters :: [Char]
possLetters = ['a'..'z']

randLetter :: MWC.GenIO -> IO Char
randLetter gen = RN.randElem possLetters gen

randLetterExcluding :: MWC.GenIO -> Char -> IO Char
randLetterExcluding gen c = do
    char <- RN.randElem possLetters gen
    if (c == char)
        then do { char' <- randLetterExcluding gen c
                ; return char' }
        else return char

randInteger :: MWC.GenIO -> IO Int
randInteger gen = RN.randInt 0 2147483647 gen

data Chunk = CharChunk   Char
           | BinaryChunk Integer Int -- val of data + num of significant bits
           deriving Show

-- Value generation: creates a list of Chunks, combined elsewhere
generateChunks :: GenType -> MWC.GenIO -> IO [Chunk]
generateChunks (Lit c)  gen = return [CharChunk c]
generateChunks (GTChar) gen = ((:[]) . CharChunk) <$> randLetter gen
generateChunks (GTInt)  gen = ((map CharChunk) <$>) show <$> randInteger gen
generateChunks (App GTBits8 (Int n)) gen = do
    when (n > 8 || n < 0)
        (error $ "Bad Bits8 value: " ++ (show n))
    r <- randInteger gen
    return $ [BinaryChunk (fromIntegral r) n]
generateChunks (App GTBits16 (Int n)) gen = do
    when (n > 16 || n < 0)
        (error $ "Bad Bits16 value: " ++ (show n))
    r <- randInteger gen
    return $ [BinaryChunk (fromIntegral r) n]
generateChunks (App GTBits32 (Int n)) gen = do
    when (n > 32 || n < 0)
        (error $ "Bad Bits32 value: " ++ (show n))
    r <- randInteger gen
    return $ [BinaryChunk (fromIntegral r) n]
generateChunks (App GTBits64 (Int n)) gen = do
    when (n > 64 || n < 0)
        (error $ "Bad Bits64 value: " ++ (show n))
    r <- MWC.uniformR (0 :: Word64, 2 ^ 64 - 1 :: Word64) gen
    return $ [BinaryChunk (fromIntegral r) n]
generateChunks (App GTStringFW (Int n)) gen =
    concat <$> replicateM (fromIntegral n) (generateChunks GTChar gen)
generateChunks (App GTStringC (Lit c)) gen = do
    len <- RN.randInt 1 listLimit gen
    str <- replicateM len (randLetterExcluding gen c)
    return $ (map CharChunk (str ++ [c]))
generateChunks (GTList ds) gen = do
    vals <- mapM (\x -> generateChunks x gen) ds
    return $ concat vals
generateChunks (Choice cs) gen = do
    rand <- RN.randElem cs gen
    generateChunks rand gen
generateChunks (x) _ = error $ "Unimplemented generation: " ++ (show x)  -- = (\x -> '_':[]) --if we have no idea what to do

-- This should be (and is) O(n), unless shiftL and shiftR take linear time
-- which they might on Integers
fromChunks :: [Chunk] -> IO [Word8]
fromChunks cs = do
    let bits = foldr getBits 0 cs
    when (bits `mod` 8 /= 0)
        (error $ "Bad total bit length: " ++ (show bits) ++ " bits described") -- Necessary? combineChunks should be robust enough
    i <- combineChunks cs bits                                                 -- to handle weird non-byte-aligned stuff
    w8s <- reverse <$> createWord8s i
    return $ if   length w8s /= bits `div` 8
             then (replicate ((bits `div` 8) - length w8s) 0) ++ w8s
             else w8s
    -- Necessary guard: with >=8 leading zeroes (generated with <=1/256 chance),
    -- createWord8s behaves improperly by stripping them

    where
        getBits :: Chunk -> Int -> Int
        getBits (CharChunk _)     z = 8 + z
        getBits (BinaryChunk _ b) z = b + z

        combineChunks :: [Chunk] -> Int -> IO Integer
        combineChunks [] _ = return 0
        combineChunks _ 0 = error "ran out of bits"
        combineChunks ((CharChunk c):cs) bs = do
            let i = ((fromIntegral . chrToWord8) c) `shiftL` (bs - 8)
            rest <- combineChunks cs (bs - 8)
            return $ i + rest
        combineChunks ((BinaryChunk v b):cs) bs = do
            let i = (fromIntegral $ v .&. (2^b - 1)) `shiftL` (bs - b)
            rest <- combineChunks cs (bs - b)
            return $ i + rest

        -- Result needs to be reversed
        createWord8s :: Integer -> IO [Word8]
        createWord8s 0 = return []
        createWord8s i = do
            let w = fromIntegral $ i .&. 255
            rest <- createWord8s (i `shiftR` 8)
            return (w:rest)


mk_render_char :: PadsDecl -> MWC.GenIO -> IO [Chunk]
mk_render_char pd gen = do
    ds   <- gen_ast pd
    vals <- mapM (\x -> generateChunks x gen) ds
    return $ concat vals

findPadsAst name lst =
    case L.find ((== name) . fst) lst
      of Just n  -> n
         Nothing -> error $ "PADS identifier " ++ (show name) ++ " not found"

res str= let
  ((x, (y, z)), s) = sTART_parseS str
  bad = numErrors y + length s
  in bad

--generate :: [Char] -> ([Char], PadsDecl) -> IO String
generate padsID padsExp = do
    gen <- MWC.createSystemRandom
    cs <- mk_render_char (snd (findPadsAst padsID padsExp)) gen
    ws <- fromChunks cs
    return $ map word8ToChr ws

generateTemp = do
    gen <- MWC.createSystemRandom
    cs <- mk_render_char (snd (findPadsAst "START" padsSamples)) gen
    ws <- fromChunks cs
    return $ (cs, map word8ToChr ws)
-- example usage: generate "START" -- creates instance from START pads description
