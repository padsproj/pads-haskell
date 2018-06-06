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
--import qualified Language.Pads.DataGen.Rand as RN
import           Language.Pads.DataGen.GenDescriptions

{-
 - General generation logic: extract relevalt info from a PADS AST, convert
 - into generation-friendly datatypes (GenType), convert into "Chunks" to
 - allow for non-byte-aligned and sub-byte generation, convert into [Word8]
 - as final raw output
 -}

--                       Name      AST
type PadsDescription = ([Char], PadsDecl)

-- Bring into scope our pads ASTs
$(make_pads_declarations $ map snd padsSamples)
envs = padsSamples

genAST :: PadsDecl -> IO [GenType]
genAST (PadsDeclData _ xs args padsData e)   = genPadsData padsData        --
genAST (PadsDeclType _ xs args padsTy)       = (:[]) <$> genPadsTy padsTy  -- Is any of this complete? What do the other args mean?
genAST (PadsDeclNew  _ xs args branchinfo e) = genBranches branchinfo      --
--genAST (PadsDeclObtain id xs padsTy e) =
genAST x = error $ "genAST: unimplemented: " ++ (show x)

genPadsData :: PadsData -> IO [GenType]
genPadsData (PUnion branches) = do
    gen <- MWC.createSystemRandom
    b   <- randElem branches gen
    b'  <- genBranches b
    let gs = GTList b'
    return [gs]
-- genPadsData (PSwitch e branches) =
genPadsData x = error $ "Error in genPadsData: " ++ (show x)

genBranches :: BranchInfo -> IO [GenType]
genBranches (BRecord _ fs e) = mapM genFieldInfo fs
genBranches (BConstr _ fs e) = mapM genConstrArg fs

genFieldInfo :: FieldInfo -> IO GenType
genFieldInfo (_, constarg, e) = genConstrArg constarg -- Can't always disregard exp

genConstrArg :: ConstrArg -> IO GenType
genConstrArg (_, padsTy) =  genPadsTy padsTy

listLimit = 20 -- Limit how long generated lists can be.


-- This function actually takes apart padsTys
genPadsTy :: PadsTy -> IO GenType
genPadsTy (PList pty delim term) = do
    t   <- genPadsTy pty
    gen <- MWC.createSystemRandom
    n   <- randIntBetween 1 listLimit gen
    case delim
      of Just d -> do
            d' <- genPadsTy d
            return $ GTList (L.intersperse d' (replicate n t))
         Nothing -> return $ GTList (replicate n t)
genPadsTy (PApp xs (Just e)) = do
    pt <- genPadsTy $ xs !! 0
    return $ App pt (gen_lit e)
genPadsTy (PExpression exp) = return $ gen_lit exp
genPadsTy (PTycon xs) = genBase $ xs !! 0
genPadsTy x = error $ "genPadsTy: unimplemented: " ++ show x

-- PConstrain Pat PadsTy Exp
-- PTransform PadsTy PadsTy Exp
----- PList PadsTy (Maybe PadsTy) (Maybe TermCond)
-- PPartition PadsTy Exp
-- PValue Exp PadsTy
----- PApp [PadsTy] (Maybe Exp)
-- PTuple [PadsTy]
----- PExpression Exp
----- PTycon QString
-- PTyvar String


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

genBase :: [Char] -> IO GenType
genBase "BitField"  = return GTBitField
genBase "Bits8"     = return GTBits8
genBase "Bits16"    = return GTBits16
genBase "Bits32"    = return GTBits32
genBase "Bits64"    = return GTBits64
genBase "Char"      = return GTChar
genBase "Int"       = return GTInt
genBase "StringFW"  = return GTStringFW
genBase "StringC"   = return GTStringC
-- this will be replaced with something which actually can find all the ASTs
-- in a single location
genBase x = (genLookup x envs)

-- If we couldn't find a base type, maybe it's another PADS type...
-- try looking it up.
genLookup :: [Char] -> [PadsDescription] -> IO GenType
genLookup s ( (s1,b):xs ) | s == s1 = GTList <$> genAST b
genLookup s ( (x,b):xs ) = genLookup s xs
genLookup s [] = error ("could not find ast_" ++ s)





randIntBetween :: Int -> Int -> MWC.GenIO -> IO Int
randIntBetween lo hi gen = MWC.uniformR (lo, hi) gen

randInt :: MWC.GenIO -> IO Int
randInt gen = randIntBetween 0 2147483647 gen

randElem :: [a] -> MWC.GenIO -> IO a
randElem xs gen = do
    r <- fromIntegral <$> randIntBetween 0 (length xs - 1) gen
    return $ xs !! r

possChars :: [Char]
possChars = ['a'..'z']

randLetter :: MWC.GenIO -> IO Char
randLetter gen = randElem possChars gen

randLetterExcluding :: MWC.GenIO -> Char -> IO Char
randLetterExcluding gen c = do
    char <- randElem possChars gen
    if (c == char)
        then do { char' <- randLetterExcluding gen c
                ; return char' }
        else return char





data Chunk = CharChunk   Char
           | BinaryChunk Integer Int -- val of data + num of significant bits
    deriving Show

-- Value generation: creates a list of Chunks, combined elsewhere
generateChunks :: GenType -> MWC.GenIO -> IO [Chunk]
generateChunks (Lit c)  gen = return [CharChunk c]
generateChunks (GTChar) gen = ((:[]) . CharChunk) <$> randLetter gen
generateChunks (GTInt)  gen = ((map CharChunk) <$>) show <$> randInt gen
generateChunks (App GTBits8 (Int n)) gen = do
    when (n > 8 || n < 0)
        (error $ "Bad Bits8 value: " ++ (show n))
    r <- randInt gen
    return $ [BinaryChunk (fromIntegral r) n]
generateChunks (App GTBits16 (Int n)) gen = do
    when (n > 16 || n < 0)
        (error $ "Bad Bits16 value: " ++ (show n))
    r <- randInt gen
    return $ [BinaryChunk (fromIntegral r) n]
generateChunks (App GTBits32 (Int n)) gen = do
    when (n > 32 || n < 0)
        (error $ "Bad Bits32 value: " ++ (show n))
    r <- randInt gen
    return $ [BinaryChunk (fromIntegral r) n]
generateChunks (App GTBits64 (Int n)) gen = do
    when (n > 64 || n < 0)
        (error $ "Bad Bits64 value: " ++ (show n))
    r <- MWC.uniformR (0 :: Word64, 2 ^ 64 - 1 :: Word64) gen
    return $ [BinaryChunk (fromIntegral r) n]
generateChunks (App GTStringFW (Int n)) gen =
    concat <$> replicateM (fromIntegral n) (generateChunks GTChar gen)
generateChunks (App GTStringC (Lit c)) gen = do
    len <- randIntBetween 1 listLimit gen
    str <- replicateM len (randLetterExcluding gen c)
    return $ (map CharChunk (str ++ [c]))
generateChunks (GTList ds) gen = do
    vals <- mapM (\x -> generateChunks x gen) ds
    return $ concat vals
generateChunks (Choice cs) gen = do
    rand <- randElem cs gen
    generateChunks rand gen
generateChunks (x) _ = error $ "generateChunks: unimplemented: " ++ (show x)  -- = (\x -> '_':[]) --if we have no idea what to do

-- This should be (and is) O(n), unless shiftL and shiftR take linear time on
-- on Integers, which they might
fromChunks :: [Chunk] -> IO [Word8]
fromChunks cs = do
    let bits = foldr getBits 0 cs
    when (bits `mod` 8 /= 0)                                                   -- Necessary? combineChunks should be robust enough
        (error $ "Bad total bit length: " ++ (show bits) ++ " bits described") -- to handle weird non-byte-aligned stuff
    i <- combineChunks cs bits
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
    ds   <- genAST pd
    vals <- mapM (\x -> generateChunks x gen) ds
    return $ concat vals

findPadsAST :: [Char] -> [PadsDescription] -> PadsDescription
findPadsAST name lst =
    case L.find ((== name) . fst) lst
      of Just n  -> n
         Nothing -> error $ "PADS identifier " ++ (show name) ++ " not found"

-- res str= let
--   ((x, (y, z)), s) = sTART_parseS str
--   bad = numErrors y + length s
--   in bad

generate :: [Char] -> [PadsDescription] -> IO [Char]
generate padsID padsExp = do
    gen <- MWC.createSystemRandom
    cs <- mk_render_char (snd (findPadsAST padsID padsExp)) gen
    ws <- fromChunks cs
    return $ map word8ToChr ws

-- example usage: generate "START" padsSamples -- creates instance from START
-- pads description in padsSamples
