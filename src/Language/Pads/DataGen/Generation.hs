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
import           Control.Monad.Reader as R
import           Data.Bits
import qualified Data.ByteString as B
import           Data.IORef
import qualified Data.List as L
import           Data.Maybe
import           Data.Word
import qualified Language.Haskell.TH as TH
import qualified System.Random.MWC as MWC
import           Text.Printf

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

data Chunk = CharChunk   Char
           | BinaryChunk Integer Int -- Value, number of significant bits of value
    deriving Show

data Val = CharVal Char
         | IntVal Integer
    deriving Show

type ValEnv = [(String, Val)]

data MEnv = MEnv { genIO     :: MWC.GenIO
                 , valEnvRef :: IORef ValEnv
                 }

type M = ReaderT MEnv IO

-- type Name = String

-- Bring into scope our pads ASTs
$(make_pads_declarations $ map snd padsSamples)
envs = padsSamples

genAST :: PadsDecl -> M [GenType]
genAST (PadsDeclData _ xs args padsData e)   = genPadsData padsData        --
genAST (PadsDeclType _ xs args padsTy)       = (:[]) <$> genPadsTy "" padsTy  -- Is any of this complete? What do the other args mean?
genAST (PadsDeclNew  _ xs args branchinfo e) = genBranches branchinfo      --
genAST x@(PadsDeclObtain _ xs padsTy e)       =
    error $ "genAST: unimplemented: PadsDeclObtain: " ++ show x

genPadsData :: PadsData -> M [GenType]
genPadsData (PUnion branches) = do
    env <- ask
    let gen = genIO env
    b   <- R.lift $ randElem branches gen
    b'  <- genBranches b
    return b'
genPadsData x@(PSwitch e branches) =
    error $ "genPadsData: unimplemented: PSwitch: " ++ show x

genBranches :: BranchInfo -> M [GenType]
genBranches (BRecord _ fs e) = mapM genFieldInfo fs
genBranches (BConstr _ fs e) = mapM (genConstrArg "") fs

genFieldInfo :: FieldInfo -> M GenType
genFieldInfo (name, constrarg, e) =
    case name of
        Just n  -> genConstrArg n  constrarg -- Can't always disregard exp
        Nothing -> genConstrArg "" constrarg

genConstrArg :: String -> ConstrArg -> M GenType
genConstrArg n (_, padsTy) =  genPadsTy n padsTy

listLimit = 20 -- Limit how long generated lists can be.


genPadsTy :: String -> PadsTy -> M GenType
genPadsTy _ x@(PConstrain pat pt e) = error $ "genPadsTy: unsupported: PConstrain: " ++ show x
genPadsTy _ x@(PTransform pt pt' e) = error $ "genPadsTy: unsupported: PTransform: " ++ show x
genPadsTy n (PList pt delim term) = do
    env <- ask
    let gen = genIO env
    gt  <- genPadsTy n pt
    i   <- R.lift $ MWC.uniformR (1, listLimit) gen
    case delim of
        Just d -> do
            d' <- genPadsTy n d
            return $ GTList (L.intersperse d' (replicate i gt))
        Nothing -> return $ GTList (replicate i gt)
genPadsTy n (PPartition pt e) = genPadsTy n pt
genPadsTy _ x@(PValue e pt) =
    error $ "genPadsTy: unsupported: PValue: " ++ show x
genPadsTy n (PApp xs (Just e)) = do
    pt <- genPadsTy n $ xs !! 0
    return $ GTNamed n $ GTApp pt e
genPadsTy n (PTuple pts) = GTList <$> mapM (genPadsTy n) pts
genPadsTy n (PExpression exp) = return $ genParam exp
genPadsTy n (PTycon xs) = --genBase n $ xs !! 0
    case xs of
        [x] -> if   any (== x) baseTypes
               then genBase n x
               else return $ GTTycon x
        _   -> error $ "genPadsTy: unsupported: qualified names: " ++ show xs
genPadsTy n x@(PTyvar y) = error $ "genPadsTy: unsupported: PTyvar: " ++ show x

baseTypes = [ "BitField"
            , "Bits8"
            , "Bits16"
            , "Bits32"
            , "Bits64"
            , "Bytes"
            , "StringFW"
            , "StringC"
            , "Char"
            , "Int"
            ]


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
             | GTTycon String
             | GTBitField -- Parameterized
             | GTBits8    -- Parameterized
             | GTBits16   -- Parameterized
             | GTBits32   -- Parameterized
             | GTBits64   -- Parameterized
             | GTBytes
             | GTStringC  -- Parameterized
             | GTStringFW -- Parameterized
             | GTChar
             | GTInt
             | GTList [ GenType ]
             | GTVar String
             | GTCharL Char -- Argument to GTApp
             | GTIntL Int   -- Argument to GTApp
             | GTApp GenType TH.Exp
             | Choice [ GenType ]

  deriving (Show)

-- Parameterized types are named elsewhere (in GTApp/PApp)
genBase :: String -> String -> M GenType
genBase _ "BitField"  = return GTBitField
genBase _ "Bits8"     = return GTBits8
genBase _ "Bits16"    = return GTBits16
genBase _ "Bits32"    = return GTBits32
genBase _ "Bits64"    = return GTBits64
genBase _ "Bytes"     = return GTBytes
genBase _ "StringFW"  = return GTStringFW
genBase _ "StringC"   = return GTStringC
genBase n "Char"      = return $ GTNamed n GTChar
genBase n "Int"       = return $ GTNamed n GTInt
-- this will be replaced with something which actually can find all the ASTs
-- in a single location
genBase _ x = genLookup x envs

-- | Locate the AST of a non-base (user-defined) PADS type
genLookup :: String -> [PadsDescription] -> M GenType
genLookup s ds =
    case L.find ((== s) . fst) ds of
        Just d  -> GTList <$> genAST (snd d)
        Nothing -> error $ "genLookup: failed lookup: " ++ s



find :: String -> M Val
find n = do
    env <- ask
    valEnv <- R.lift $ readIORef $ valEnvRef env
    case L.find ((== n) . fst) valEnv of
        Just (_, val) -> return val
        Nothing       -> error $ "find: failed lookup: " ++ n

bind :: String -> Val -> M ()
bind n v = do
    env <- ask
    valEnv <- R.lift $ readIORef $ valEnvRef env
    R.lift $ writeIORef (valEnvRef env) $ (n,v):valEnv



-- Value generation: creates a list of Chunks, combined elsewhere
generateChunks :: GenType -> M [Chunk]
generateChunks (GTNamed n gt) = do
    cs <- generateChunks gt
    case cs of
        [CharChunk c] -> do
            bind n (CharVal c)
            return [CharChunk c]
        [BinaryChunk v b] -> do
            let val = IntVal $ v .&. (2^b - 1)
            bind n val
            return [BinaryChunk v b]
        _ -> return cs
generateChunks (GTCharL c) = return [CharChunk c]
generateChunks (GTIntL i)  = return $ map CharChunk $ show i
generateChunks GTChar = do
    env <- ask
    R.lift $ (:[]) <$> CharChunk <$> randLetter (genIO env)
generateChunks GTInt = do
    env <- ask
    R.lift $ map CharChunk <$> show <$> randInt (genIO env)
generateChunks (GTTycon x) = genBase "" x >>= generateChunks
generateChunks (GTApp (GTTycon x) e) = do
    --printf "generateChunks: looking up %s\n" x
    let ast = snd $ findPadsAST x
    lit <- case e of
        TH.LitE _ -> return e
        TH.VarE n -> do
            let n' = TH.nameBase n
            v <- find n'
            return $ case v of
                CharVal c -> TH.LitE $ TH.CharL c
                IntVal  i -> TH.LitE $ TH.IntegerL $ fromIntegral i
        _ -> error $ "generateChunks: GTApp: unsupported parameter: " ++ show e
    let ast' = replaceVars (paramOf ast) lit ast
    -- substitution of literal value for variable name
    gt' <- genAST ast'
    generateChunks (GTList gt')
    -- generation

    where
        replaceVars :: TH.Exp -> TH.Exp -> PadsDecl -> PadsDecl
        replaceVars var lit (PadsDeclData a b c d e) = PadsDeclData a b c (replaceVarsInPadsData var lit d) e
        replaceVars var lit (PadsDeclType a b c d)   = PadsDeclType a b c (replaceVarsInPadsTy var lit d)
        replaceVars var lit (PadsDeclNew  a b c d e) = PadsDeclNew  a b c (replaceVarsInBranchInfo var lit d) e

        replaceVarsInPadsData :: TH.Exp -> TH.Exp -> PadsData -> PadsData
        replaceVarsInPadsData var lit (PUnion branches) = PUnion $ map (replaceVarsInBranchInfo var lit) branches

        replaceVarsInPadsTy :: TH.Exp -> TH.Exp -> PadsTy -> PadsTy
        replaceVarsInPadsTy var lit (PApp x (Just e)) = if   var == e
                                                        then PApp x (Just lit)
                                                        else PApp x (Just e)
        replaceVarsInPadsTy var lit x = x

        replaceVarsInBranchInfo :: TH.Exp -> TH.Exp -> BranchInfo -> BranchInfo
        replaceVarsInBranchInfo var lit (BRecord a b c) = BRecord a (map (replaceVarsInFieldInfo var lit) b) c
        replaceVarsInBranchInfo var lit (BConstr a b c) = BConstr a (map (replaceVarsInConstrArg var lit) b) c

        replaceVarsInFieldInfo :: TH.Exp -> TH.Exp -> FieldInfo -> FieldInfo
        replaceVarsInFieldInfo var lit (a, b, c) = (a, replaceVarsInConstrArg var lit b, c)

        replaceVarsInConstrArg :: TH.Exp -> TH.Exp -> ConstrArg -> ConstrArg
        replaceVarsInConstrArg var lit (a, b) = (a, replaceVarsInPadsTy var lit b)

        paramOf :: PadsDecl -> TH.Exp
        paramOf (PadsDeclData _ _ (Just (TH.ParensP (TH.SigP (TH.VarP x) (TH.ConT t)))) _ _) = TH.VarE x
        paramOf (PadsDeclType _ _ (Just (TH.ParensP (TH.SigP (TH.VarP x) (TH.ConT t)))) _)   = TH.VarE x
        paramOf (PadsDeclNew  _ _ (Just (TH.ParensP (TH.SigP (TH.VarP x) (TH.ConT t)))) _ _) = TH.VarE x

generateChunks (GTApp gt (TH.LitE (TH.IntegerL i))) = do
    env <- ask
    v <- MWC.uniformR (0 :: Word64, 2^64 - 1 :: Word64) (genIO env)
    case gt of
        GTBits8 ->
            if   (i > 8 || i < 0)
            then error $ "generateChunks: bad Bits8 value: " ++ show i
            else return $ [BinaryChunk (fromIntegral v) (fromIntegral i)]
        GTBits16 ->
            if   (i > 16 || i < 0)
            then error $ "generateChunks: bad Bits16 value: " ++ show i
            else return $ [BinaryChunk (fromIntegral v) (fromIntegral i)]
        GTBits32 ->
            if   (i > 32 || i < 0)
            then error $ "generateChunks: bad Bits32 value: " ++ show i
            else return $ [BinaryChunk (fromIntegral v) (fromIntegral i)]
        GTBits64 ->
            if   (i > 64 || i < 0)
            then error $ "generateChunks: bad Bits64 value: " ++ show i
            else return $ [BinaryChunk (fromIntegral v) (fromIntegral i)]
        GTStringFW ->
            concat <$> replicateM (fromIntegral i) (generateChunks GTChar)
        GTBytes -> do
            bs <- R.lift $ replicateM (fromIntegral i) (MWC.uniformR (0 :: Word8, 255 :: Word8) (genIO env))
            return $ map (CharChunk . word8ToChr) bs
        x -> error $ "generateChunks: unsupported generation type: " ++ show x
generateChunks (GTApp gt (TH.LitE (TH.CharL c))) = do
    env <- ask
    case gt of
        GTStringC -> do
            len <- MWC.uniformR (0, listLimit) (genIO env)
            str <- R.lift $ replicateM len (randLetterExcluding c (genIO env))
            return $ (map CharChunk (str ++ [c]))
generateChunks (GTApp gt (TH.VarE n)) = do
    let n' = TH.nameBase n
    v <- find n'
    case v of
        CharVal c -> generateChunks (GTApp gt (TH.LitE (TH.CharL c)))
        IntVal  i -> generateChunks (GTApp gt (TH.LitE (TH.IntegerL i)))
generateChunks (GTList ds) = do
    vals <- mapM generateChunks ds
    return $ concat vals
generateChunks x = error $ "generateChunks: unimplemented: " ++ show x

-- This should be (and is) O(n), unless shiftL and shiftR take linear time on
-- on Integers, which they might
fromChunks :: [Chunk] -> M [Word8]
fromChunks cs = do
    let bits = foldr getBits 0 cs
    -- when (bits `mod` 8 /= 0)                                                   -- TODO: Necessary? combineChunks should be robust enough
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

        combineChunks :: [Chunk] -> Int -> M Integer
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
        createWord8s :: Integer -> M [Word8]
        createWord8s 0 = return []
        createWord8s i = do
            let w = fromIntegral $ i .&. 255
            rest <- createWord8s (i `shiftR` 8)
            return (w:rest)

findPadsAST :: [Char] -> PadsDescription
findPadsAST name =
    case L.find ((== name) . fst) padsSamples
      of Just n  -> n
         Nothing -> error $ "PADS identifier " ++ (show name) ++ " not found"

run :: [Char] -> M [Char]
run padsID = do
    let ast = snd $ findPadsAST padsID
    gentypes <- genAST ast
    chunks <- mapM generateChunks gentypes
    ws <- fromChunks $ concat chunks
    return $ map word8ToChr ws


-- | Overall driver function. Usage: generate "Name", where Name is the name
-- given to a PADS declaration.
--
-- TODO: probably best to make this produce a bytestring

-- [pads| data Foo = Foo { x :: Int } |]
-- generate "Foo"
generate :: String -> IO [Char]
generate padsID = do
    gen <- MWC.createSystemRandom
    env <- newIORef []
    let menv = MEnv gen env
    runReaderT (run padsID) menv
