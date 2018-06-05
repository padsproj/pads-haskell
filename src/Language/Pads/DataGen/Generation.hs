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


--import Addrs


import qualified Language.Haskell.TH as TH

--import Language.Pads.Testing
--import System.IO.Unsafe (unsafePerformIO)

import Control.Monad
import Data.Word
import Data.Bits
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.List as List
import qualified System.Random.MWC as MWC

import Language.Pads.Padsc
import qualified Language.Pads.DataGen.Rand as RN
import Language.Pads.DataGen.GenDescriptions

-- Bring into scope our pads ASTs
$(make_pads_declarations $ map snd padsSamples)
envs = padsSamples

gen_ast :: PadsDecl -> IO [GENTYPE]
gen_ast (PadsDeclData id xs args padsdatas e) = gen_padsdatas padsdatas --  String [String] (Maybe Pat) PadsData [QString]
gen_ast (PadsDeclType id xs args padsty)      = (:[]) <$> gen_padsty padsty
gen_ast x = error $ "Error in gen_ast:  " ++ (show x)


gen_padsdatas (PUnion branches) = do
    bs <- mapM gen_branches branches --generate for each branch
    let gs = map GTLIST bs               --make each branch a list
    return [CHOICE gs] --embed in a CHOICE type
gen_padsdatas x = error $ "Error in gen_padsdatas: " ++ (show x)


gen_branches (BRecord id fs exp) =  mapM gen_fieldinfo fs
gen_branches (BConstr id fs e)   =  mapM gen_constarg fs

gen_fieldinfo ((id, constarg, exp)) = gen_constarg constarg

gen_constarg  ((strict, padsty)) =  gen_padsty padsty

listLimit = 20 -- Limit how long generated lists can be.


-- This function actually takes apart padsty's
gen_padsty :: PadsTy -> IO GENTYPE
gen_padsty (PTycon xs)            = gen_base $ xs !! 0
gen_padsty (PExpression exp)      = return $ gen_lit exp
--gen_padsty (PTuple xs)            =
gen_padsty (PApp xs (Just e))     = do
    pt <- gen_padsty $ xs !! 0
    print "App"
    return $ APP pt (gen_lit e)
gen_padsty (PList pty delim term) = do
    t   <- gen_padsty pty
    gen <- MWC.createSystemRandom
    n   <- RN.randInt 1 listLimit gen
    case delim
      of Just d -> do
            d' <- gen_padsty d
            return $ GTLIST (List.intersperse d' (replicate n t))
         Nothing -> return $ GTLIST (replicate n t)
gen_padsty x = error $ "Error in gen_padsty: " ++ show x


-- step out into template haskell to get the literal value we need
-- and turn it into a GENTYPE
gen_lit (TH.LitE (TH.CharL x))    = (LIT x)
gen_lit (TH.LitE (TH.StringL xs)) = (GTLIST (map LIT xs) ) -- treat string as a list of chars
gen_lit (TH.LitE (TH.IntegerL x)) = (INT (fromIntegral x))



data GENTYPE =  GTBITFIELD -- parameterized
              | GTBITS8    -- parameterized
              | GTBITS16   -- parameterized
              | GTBITS32   -- parameterized
              | GTBITS64   -- parameterized
              | GTSTRINGC  -- parameterized
              | GTSTRINGFW -- parameterized
              | GTCHAR
              | GTINT
              | GTLIST [ GENTYPE ]
              | LIT Char    --Arguments to APP
              | INT Int --Arguments to APP
              | APP GENTYPE GENTYPE
              | CHOICE [ GENTYPE ]
  deriving (Show)

gen_base "Char"      = return GTCHAR
gen_base "Int"       = return GTINT
gen_base "StringFW"  = return GTSTRINGFW
gen_base "BitField"  = return GTBITFIELD
gen_base "Bits8"     = return GTBITS8
gen_base "Bits16"    = return GTBITS16
gen_base "Bits32"    = return GTBITS32
gen_base "Bits64"    = return GTBITS64
gen_base "StringC"   = return GTSTRINGC
-- this will be replaced with something which actually can find all the ASTs
-- in a single location
gen_base x = (gen_lookup x envs)

-- If we couldn't find a base type, maybe it's another PADS type...
-- try looking it up.
gen_lookup s ( (s1,b):xs ) | s == s1 = GTLIST <$> gen_ast b
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

-- Value generation, creates a list of Chunks, combined elsewhere
generateChunks :: GENTYPE -> MWC.GenIO -> IO [Chunk]
generateChunks (LIT c)  gen = return [CharChunk c]               -- If we have a constant, produce it.
generateChunks (GTCHAR) gen = ((:[]) . CharChunk) <$> randLetter gen
generateChunks (GTINT)  gen = ((map CharChunk) <$>) show <$> randInteger gen
generateChunks (APP GTBITS8 (INT n)) gen = do
    when (n > 8 || n < 0)
        (error $ "Bad Bits8 value: " ++ (show n))
    r <- randInteger gen
    return $ [BinaryChunk (fromIntegral r) n]
generateChunks (APP GTBITS16 (INT n)) gen = do
    when (n > 16 || n < 0)
        (error $ "Bad Bits16 value: " ++ (show n))
    r <- randInteger gen
    return $ [BinaryChunk (fromIntegral r) n]
generateChunks (APP GTBITS32 (INT n)) gen = do
    when (n > 32 || n < 0)
        (error $ "Bad Bits32 value: " ++ (show n))
    r <- randInteger gen
    return $ [BinaryChunk (fromIntegral r) n]
generateChunks (APP GTBITS64 (INT n)) gen = do
    when (n > 64 || n < 0)
        (error $ "Bad Bits64 value: " ++ (show n))
    r <- MWC.uniformR (0 :: Word64, 2 ^ 64 - 1 :: Word64) gen
    return $ [BinaryChunk (fromIntegral r) n]
generateChunks (APP GTSTRINGFW (INT n)) gen =
    concat <$> replicateM (fromIntegral n) (generateChunks GTCHAR gen)
generateChunks (APP GTSTRINGC (LIT c)) gen = do
    len <- RN.randInt 1 listLimit gen
    str <- replicateM len (randLetterExcluding gen c)
    return $ (map CharChunk (str ++ [c]))
generateChunks (GTLIST ds) gen = do
    vals <- mapM (\x -> generateChunks x gen) ds
    return $ concat vals
generateChunks (CHOICE cs) gen = do
    rand <- RN.randElem cs gen
    generateChunks rand gen
generateChunks (x) _ = error $ "Unimplemented generation: " ++ (show x)  -- = (\x -> '_':[]) --if we have no idea what to do

fromChunks :: [Chunk] -> IO [Word8]
fromChunks cs = do
    let len = foldr getBits 0 cs
    when (len `mod` 8 /= 0)
        (error $ "Bad total bit length: " ++ (show len) ++ " bits described")
    i <- combineChunks cs len
    w8s <- reverse <$> createWord8s i
    return w8s

    where
        getBits :: Chunk -> Int -> Int
        getBits (CharChunk _)     z = 8 + z
        getBits (BinaryChunk _ b) z = b + z

        combineChunks :: [Chunk] -> Int -> IO Integer
        combineChunks [] _ = return 0
        combineChunks _ 0 = error "combineChunks: ran out of bits?"
        combineChunks ((CharChunk c):cs) bs = do
            let i = ((fromIntegral . chrToWord8) c) `shiftL` (bs - 8)
            rest <- combineChunks cs (bs - 8)
            return $ i + rest
        combineChunks ((BinaryChunk v b):cs) bs = do
            let i = (fromIntegral $ v .&. (2^b - 1)) `shiftL` (bs - b)
            rest <- combineChunks cs (bs - b)
            return $ i + rest

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
    case List.find (((== name) . fst )) lst
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
-- example usage: generate "START" -- creates instance from START pads description
