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
import           Control.Monad.Reader
import           Data.Bits
import qualified Data.ByteString as B
import           Data.IORef
import qualified Data.List as L
import           Data.Maybe
import           Data.Word
import qualified Language.Haskell.TH as TH
import qualified System.Random.MWC as MWC

import           Language.Pads.Padsc
import           Language.Pads.DataGen.GenDescriptions
import           Language.Pads.DataGen.Rand

{-
 - General generation logic: extract relevant info from a PADS AST, convert
 - into generation-friendly datatypes (GenType), convert into "Chunks" to
 - allow for non-byte-aligned and sub-byte generation, convert into [Word8]
 - as final raw output
 -}

--                       Name      AST
type PadsDescription = ([Char], PadsDecl)

-- type Name = String

-- Bring into scope our pads ASTs
$(make_pads_declarations $ map snd padsSamples)
envs = padsSamples

genAST :: PadsDecl -> IO [GenType]
genAST (PadsDeclData _ xs args padsData e)   = genPadsData padsData        --
genAST (PadsDeclType _ xs args padsTy)       = (:[]) <$> genPadsTy "" padsTy  -- Is any of this complete? What do the other args mean?
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
genPadsData x = error $ "genPadsData: unimplemented: " ++ show x

genBranches :: BranchInfo -> IO [GenType]
genBranches (BRecord _ fs e) = mapM genFieldInfo fs
genBranches (BConstr _ fs e) = mapM (genConstrArg "") fs

genFieldInfo :: FieldInfo -> IO GenType
genFieldInfo (name, constrarg, e) =
    case name of
        Just n  -> genConstrArg n  constrarg -- Can't always disregard exp
        Nothing -> genConstrArg "" constrarg

genConstrArg :: String -> ConstrArg -> IO GenType
genConstrArg n (_, padsTy) =  genPadsTy n padsTy

listLimit = 20 -- Limit how long generated lists can be.


genPadsTy :: String -> PadsTy -> IO GenType
genPadsTy n (PList pt delim term) = do
    gt  <- genPadsTy n pt
    gen <- MWC.createSystemRandom
    i   <- MWC.uniformR (1, listLimit) gen
    case delim of
        Just d -> do
            d' <- genPadsTy n d
            return $ GTList (L.intersperse d' (replicate i gt))
        Nothing -> return $ GTList (replicate i gt)
genPadsTy n (PPartition pt e) = genPadsTy n pt
genPadsTy n (PApp xs (Just e)) = do
    pt <- genPadsTy n $ xs !! 0
    return $ GTNamed n $ App pt (genParam e)
genPadsTy n (PTuple pts) = GTList <$> mapM (genPadsTy n) pts
genPadsTy n (PExpression exp) = return $ genParam exp
genPadsTy n (PTycon xs) = genBase n $ xs !! 0
genPadsTy n (PTyvar x) = error $ "genPadsTy: PTyvar: generation unsupported"
genPadsTy n x = error $ "genPadsTy: unimplemented: " ++ show x

-- PConstrain Pat PadsTy Exp
-- PTransform PadsTy PadsTy Exp
----- PList PadsTy (Maybe PadsTy) (Maybe TermCond)
----- PPartition PadsTy Exp
-- PValue Exp PadsTy
----- PApp [PadsTy] (Maybe Exp)
----- PTuple [PadsTy]
----- PExpression Exp
----- PTycon QString
----- PTyvar String

-- | Some PADS descriptions are parameterized with a Haskell expression; this
-- function parses such expressions
genParam :: TH.Exp -> GenType
genParam (TH.LitE (TH.CharL c))    = GTCharL c
genParam (TH.LitE (TH.StringL cs)) = GTList $ map GTCharL cs -- treat string as a list of chars
genParam (TH.LitE (TH.IntegerL i)) = GTIntL $ fromIntegral i
genParam (TH.VarE n)               = GTVar $ TH.nameBase n
genParam x = error $ "genParam: unimplemented: " ++ show x
-- To support more complex parameterization (e.g. Bits8 <| len * 5 |>),
-- we require interpretation capabilities of TemplateHaskell. As of
-- now, only simple variable names are supported.
-- Possible future adaptations: simple arithmetic, use of fromIntegral

data GenType = GTNamed String GenType
             | GTBitField -- Parameterized
             | GTBits8    -- Parameterized
             | GTBits16   -- Parameterized
             | GTBits32   -- Parameterized
             | GTBits64   -- Parameterized
             | GTStringC  -- Parameterized
             | GTStringFW -- Parameterized
             | GTChar
             | GTInt
             | GTList [ GenType ]
             | GTVar String
             | GTCharL Char -- Argument to App
             | GTIntL Int   -- Argument to App
             | App GenType GenType
             | Choice [ GenType ]

  deriving (Show)

-- Parameterized types are named elsewhere (in App/PApp)
genBase :: String -> String -> IO GenType
genBase _ "BitField"  = return GTBitField
genBase _ "Bits8"     = return GTBits8
genBase _ "Bits16"    = return GTBits16
genBase _ "Bits32"    = return GTBits32
genBase _ "Bits64"    = return GTBits64
genBase _ "StringFW"  = return GTStringFW
genBase _ "StringC"   = return GTStringC
genBase n "Char"      = return $ GTNamed n GTChar
genBase n "Int"       = return $ GTNamed n GTInt
-- this will be replaced with something which actually can find all the ASTs
-- in a single location
genBase _ x = (genLookup x envs)

-- | Locate the AST of a non-base (user-defined) PADS type
genLookup :: String -> [PadsDescription] -> IO GenType
genLookup s ds =
    case L.find ((== s) . fst) ds of
        Just d  -> GTList <$> genAST (snd d)
        Nothing -> error ("could not find ast_" ++ s)



data Chunk = CharChunk   Char
           | BinaryChunk Integer Int -- Value, number of significant bits of value
    deriving Show

data Val = CharVal Char
         | IntVal Integer
    deriving Show

type ValEnv = [(String, Val)]

data MEnv = MEnv { genIO :: MWC.GenIO
                 , valEnvRef :: IORef ValEnv }

find :: String -> MEnv -> IO Val
find n e = do
    valEnv <- readIORef $ valEnvRef e
    case L.find ((== n) . fst) valEnv of
        Just (_, val) -> return val
        Nothing       -> error "find: failed lookup"

bind :: String -> Val -> MEnv -> IO ()
bind n v e = do
    valEnv <- readIORef $ valEnvRef e
    writeIORef (valEnvRef e) $ (n,v):valEnv



-- Value generation: creates a list of Chunks, combined elsewhere
generateChunks :: GenType -> MEnv -> IO [Chunk]
generateChunks (GTNamed n gt) env = do
    cs <- generateChunks gt env
    case cs of
        [CharChunk c] -> do
            bind n (CharVal c) env
            return [CharChunk c]
        [BinaryChunk v b] -> do
            let val = IntVal $ v .&. (2^b - 1)
            print val
            bind n val env
            return [BinaryChunk v b]
        _ -> return cs
generateChunks (GTCharL c) _ = return [CharChunk c]
generateChunks (GTChar) env = ((:[]) . CharChunk) <$> randLetter (genIO env)
generateChunks (GTInt) env = ((map CharChunk) <$>) show <$> randInt (genIO env)
generateChunks (App gt (GTIntL i)) env = do
    v <- MWC.uniformR (0 :: Word64, 2^64 - 1 :: Word64) (genIO env)
    case gt of
        GTBits8 ->
            if   (i > 8 || i < 0)
            then error $ "generateChunks: bad Bits8 value: " ++ show i
            else return $ [BinaryChunk (fromIntegral v) i]
        GTBits16 ->
            if   (i > 16 || i < 0)
            then error $ "generateChunks: bad Bits16 value: " ++ show i
            else return $ [BinaryChunk (fromIntegral v) i]
        GTBits32 ->
            if   (i > 32 || i < 0)
            then error $ "generateChunks: bad Bits32 value: " ++ show i
            else return $ [BinaryChunk (fromIntegral v) i]
        GTBits64 ->
            if   (i > 64 || i < 0)
            then error $ "generateChunks: bad Bits64 value: " ++ show i
            else return $ [BinaryChunk (fromIntegral v) i]
        GTStringFW ->
            concat <$> replicateM (fromIntegral i) (generateChunks GTChar env)
generateChunks (App gt (GTCharL c)) env = do
    case gt of
        GTStringC -> do
            len <- MWC.uniformR (0, listLimit) (genIO env)
            str <- replicateM len (randLetterExcluding c (genIO env))
            return $ (map CharChunk (str ++ [c]))
generateChunks (App gt (GTVar n)) env = do
    v <- find n env
    case v of
        CharVal c -> generateChunks (App gt (GTCharL c)) env
        IntVal  i -> generateChunks (App gt (GTIntL  (fromIntegral i))) env
generateChunks (GTList ds) env = do
    vals <- mapM (\x -> generateChunks x env) ds
    return $ concat vals
generateChunks (Choice cs) env = do
    rand <- randElem cs (genIO env)
    generateChunks rand env
generateChunks (x) _ = error $ "generateChunks: unimplemented: " ++ show x  -- = (\x -> '_':[]) --if we have no idea what to do

-- This should be (and is) O(n), unless shiftL and shiftR take linear time on
-- on Integers, which they might
fromChunks :: [Chunk] -> IO [Word8]
fromChunks cs = do
    let bits = foldr getBits 0 cs
    -- when (bits `mod` 8 /= 0)                                                   -- Necessary? combineChunks should be robust enough
    --     (error $ "Bad total bit length: " ++ (show bits) ++ " bits described") -- to handle weird non-byte-aligned stuff
    i <- combineChunks cs bits
    w8s <- reverse <$> createWord8s i
    -- return guard: when >=8 leading zeroes are randomly generated,
    -- createWord8s behaves improperly by stripping them - add them back in here
    return $ if   length w8s /= bits `div` 8
             then (replicate ((bits `div` 8) - length w8s) 0) ++ w8s
             else w8s

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

        -- Result will need to be reversed
        createWord8s :: Integer -> IO [Word8]
        createWord8s 0 = return []
        createWord8s i = do
            let w = fromIntegral $ i .&. 255
            rest <- createWord8s (i `shiftR` 8)
            return (w:rest)


mk_render_char :: PadsDecl -> MEnv -> IO [Chunk]
mk_render_char pd env = do
    ds   <- genAST pd
    let g = genIO env
    vals <- mapM (\x -> generateChunks x env) ds
    return $ concat vals

padsDesc = padsSamples

findPadsAST :: [Char] -> PadsDescription
findPadsAST name =
    case L.find ((== name) . fst) padsSamples
      of Just n  -> n
         Nothing -> error $ "PADS identifier " ++ (show name) ++ " not found"

-- res str= let
--   ((x, (y, z)), s) = sTART_parseS str
--   bad = numErrors y + length s
--   in bad

generate :: [Char] -> IO [Char]
generate padsID = do
    g <- MWC.createSystemRandom
    e <- newIORef []
    cs <- mk_render_char (snd (findPadsAST padsID)) (MEnv g e)
    ws <- fromChunks cs
    return $ map word8ToChr ws



-- example usage: generate "START" padsSamples -- creates instance from START
-- pads description in padsSamples
