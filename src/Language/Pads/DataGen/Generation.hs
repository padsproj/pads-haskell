{- LANGUAGE TypeFamilies
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
{-# LANGUAGE DeriveDataTypeable
           , FlexibleInstances
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , TemplateHaskell
           , TypeFamilies
           , TypeSynonymInstances #-}

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



newMEnv = do
    toplevelgen <- MWC.createSystemRandom
    toplevelenv <- newIORef []
    return $ MEnv toplevelgen toplevelenv
-- type Name = String

-- Bring into scope our pads ASTs
$(make_pads_declarations $ map snd padsSamples)
envs = padsSamples

-- Convert PADS AST to list of GenTypes, more or less a simplified AST more
-- conducive to value generation
-- TODO: consider other arguments, esp. TH expressions
genAST :: PadsDecl -> M [GenType]
genAST (PadsDeclData _ xs args padsData e)   = genPadsData padsData
genAST (PadsDeclType _ xs args padsTy)       = (:[]) <$> genPadsTy padsTy
genAST (PadsDeclNew  _ xs args branchinfo e) = genBranches branchinfo
genAST x@(PadsDeclObtain _ xs padsTy e)      =
    error $ "genAST: unimplemented: PadsDeclObtain: " ++ show x

genPadsData :: PadsData -> M [GenType]
genPadsData (PUnion branches) = do
    env <- ask
    let gen = genIO env
    b <- R.lift $ randElem branches gen
    b' <- genBranches b
    return b'
genPadsData x@(PSwitch e branches) =
    error $ "genPadsData: unimplemented: PSwitch: " ++ show x

genBranches :: BranchInfo -> M [GenType]
genBranches (BRecord _ fs e) = mapM genFieldInfo fs
genBranches (BConstr _ fs e) = mapM (genConstrArg Nothing) fs

-- TODO: consider TH exp
genFieldInfo :: FieldInfo -> M GenType
genFieldInfo (name, constrarg, e) = genConstrArg name constrarg

genConstrArg :: Maybe String -> ConstrArg -> M GenType
genConstrArg n (_, padsTy) = case n of
    Just n' -> do
        gt <- genPadsTy padsTy
        return $ GTNamed n' gt
    Nothing -> genPadsTy padsTy

listLimit = 20 -- Limit how long generated lists can be.


genPadsTy :: PadsTy -> M GenType
genPadsTy x@(PConstrain pat pt e) = error $ "genPadsTy: unsupported: PConstrain: " ++ show x
genPadsTy x@(PTransform pt pt' e) = error $ "genPadsTy: unsupported: PTransform: " ++ show x
genPadsTy (PList pt delim term) = do
    env <- ask
    let gen = genIO env
    gt  <- genPadsTy pt
    i   <- R.lift $ MWC.uniformR (1, listLimit) gen
    case delim of
        Just d -> do
            d' <- genPadsTy d
            return $ GTList (L.intersperse d' (replicate i gt))
        Nothing -> return $ GTList (replicate i gt)
genPadsTy (PPartition pt e) = genPadsTy pt
genPadsTy x@(PValue e pt) =
    error $ "genPadsTy: unsupported: PValue: " ++ show x
genPadsTy (PApp xs (Just e)) = do
    pt <- genPadsTy $ xs !! 0
    return $ GTApp pt e
genPadsTy (PTuple pts) = GTList <$> mapM (genPadsTy) pts
genPadsTy (PExpression exp) = return $ genExp exp
    where
        genExp :: TH.Exp -> GenType
        genExp (TH.LitE (TH.CharL c))    = GTCharL c
        genExp (TH.LitE (TH.StringL cs)) = GTList $ map GTCharL cs -- treat string as a list of chars
        genExp (TH.LitE (TH.IntegerL i)) = GTIntL $ fromIntegral i
        genExp (TH.VarE n)               = GTVar $ TH.nameBase n
        genExp x = error $ "genExp: unimplemented: " ++ show x
genPadsTy (PTycon xs) =
    case xs of
        [x] -> if   any (== x) baseTypes
               then genBase x
               else return $ GTTycon x
        _   -> error $ "genPadsTy: unsupported: qualified names: " ++ show xs
    where
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
genPadsTy x@(PTyvar y) = error $ "genPadsTy: unsupported: PTyvar: " ++ show x




data GenType = GTNamed String GenType
             | GTTycon String
             | GTBitField -- Parameterized
             | GTBits8    -- Parameterized
             | GTBits16   -- Parameterized
             | GTBits32   -- Parameterized
             | GTBits64   -- Parameterized
             | GTBytes    -- Parameterized
             | GTStringC  -- Parameterized
             | GTStringFW -- Parameterized
             | GTChar
             | GTInt
             | GTList [GenType]
             | GTVar String
             | GTCharL Char -- Argument to GTApp
             | GTIntL Int   -- Argument to GTApp
             | GTApp GenType TH.Exp
  deriving (Show)

genBase :: String -> M GenType
genBase "BitField"  = return GTBitField
genBase "Bits8"     = return GTBits8
genBase "Bits16"    = return GTBits16
genBase "Bits32"    = return GTBits32
genBase "Bits64"    = return GTBits64
genBase "Bytes"     = return GTBytes
genBase "StringFW"  = return GTStringFW
genBase "StringC"   = return GTStringC
genBase "Char"      = return GTChar
genBase "Int"       = return GTInt
genBase x           = genLookup x envs
    where
        genLookup :: String -> [PadsDescription] -> M GenType
        genLookup s ds =
            case lookup s ds of
                Just d  -> GTList <$> genAST d
                Nothing -> error $ "genLookup: failed lookup: " ++ s


find :: String -> M Val
find n = do
    env <- ask
    valEnv <- R.lift $ readIORef $ valEnvRef env
    case lookup n valEnv of
        Just val -> return val
        Nothing  -> error $ "find: failed lookup: " ++ n

bind :: String -> Val -> M ()
bind n v = do
    env <- ask
    valEnv <- R.lift $ readIORef $ valEnvRef env
    R.lift $ writeIORef (valEnvRef env) $ (n,v):valEnv



-- Create a list of Chunks, a datatype representing generated data which eases
-- the combination of it into a final result
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
generateChunks (GTTycon x) = genBase x >>= generateChunks
-- TODO: applying GTTycon to a fromIntegral expression will result in an error,
-- though this behavior is well-defined for base types
generateChunks (GTApp (GTTycon x) e) = do
    let ast = findPadsAST x
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
    case gt of
        GTBits8 -> do
            v <- MWC.uniformR (0 :: Word8, 2^8 - 1 :: Word8) (genIO env)
            when (i > 8 || i < 0)
                (error $ "generateChunks: bad Bits8 value: " ++ show i)
            return $ [BinaryChunk (fromIntegral v) (fromIntegral i)]
        GTBits16 -> do
            v <- MWC.uniformR (0 :: Word16, 2^16 - 1 :: Word16) (genIO env)
            when (i > 16 || i < 0)
                (error $ "generateChunks: bad Bits16 value: " ++ show i)
            return $ [BinaryChunk (fromIntegral v) (fromIntegral i)]
        GTBits32 -> do
            v <- MWC.uniformR (0 :: Word32, 2^32 - 1 :: Word32) (genIO env)
            when (i > 32 || i < 0)
                (error $ "generateChunks: bad Bits32 value: " ++ show i)
            return $ [BinaryChunk (fromIntegral v) (fromIntegral i)]
        GTBits64 -> do
            v <- MWC.uniformR (0 :: Word64, 2^64 - 1 :: Word64) (genIO env)
            when (i > 64 || i < 0)
                (error $ "generateChunks: bad Bits64 value: " ++ show i)
            return $ [BinaryChunk (fromIntegral v) (fromIntegral i)]
        GTStringFW ->
            concat <$> replicateM (fromIntegral i) (generateChunks GTChar)
        GTBytes -> do
            bs <- R.lift $ replicateM (fromIntegral i) rand8
            return $ map (CharChunk . word8ToChr) bs
            where
                rand8 :: IO Word8
                rand8 = MWC.uniformR (0 :: Word8, 255 :: Word8) (genIO env)
        x -> error $ "generateChunks: unsupported application to Int: " ++ show x
generateChunks (GTApp gt (TH.LitE (TH.CharL c))) = do
    env <- ask
    case gt of
        GTStringC -> do
            len <- MWC.uniformR (0, listLimit) (genIO env)
            str <- R.lift $ replicateM len (randLetterExcluding c (genIO env))
            return $ (map CharChunk (str ++ [c]))
        x -> error $ "generateChunks: unsupported application to Char: " ++ show x
generateChunks (GTApp gt (TH.VarE n)) = do
    let n' = TH.nameBase n
    v <- find n'
    case v of
        CharVal c -> generateChunks (GTApp gt (TH.LitE (TH.CharL c)))
        IntVal  i -> generateChunks (GTApp gt (TH.LitE (TH.IntegerL i)))
generateChunks (GTApp gt (TH.AppE (TH.VarE fromI) (TH.LitE (TH.IntegerL i)))) =
    generateChunks (GTApp gt (TH.LitE (TH.IntegerL (fromIntegral i))))
    where
        fromI = TH.mkName "fromIntegral"
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

findPadsAST :: [Char] -> PadsDecl
findPadsAST name =
    case lookup name padsSamples
      of Just n  -> n
         Nothing -> error $ "PADS identifier " ++ (show name) ++ " not found"

run :: [Char] -> M [Char]
run padsID = do
    let ast = findPadsAST padsID
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
