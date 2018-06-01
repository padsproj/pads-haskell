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


module GENINV where

import Language.Pads.Padsc
import GEN
import Addrs


import qualified Language.Haskell.TH as TH

--import Language.Pads.Testing
import Data.Word
import Data.Maybe
--import System.IO.Unsafe (unsafePerformIO)

import Control.Monad
import qualified Data.ByteString as B
import qualified Data.Word as W
import qualified Data.List as List
import qualified Data.Bits as BTS
import qualified Rand as RN
import qualified System.Random.MWC as MWC

-- Bring into scope our pads ASTs
$(make_pads_declarations $ map snd padsExp)
envs = padsExp

gen_ast :: PadsDecl -> IO [GENTYPE]
gen_ast (PadsDeclData id xs args padsdatas e) = gen_padsdatas padsdatas --  String [String] (Maybe Pat) PadsData [QString]
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

listLimit = 50 -- Limit how long generated lists can be.


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
gen_lit (TH.LitE (TH.IntegerL x)) = (INT x)



data GENTYPE =  GTBITFIELD  --Bitfield (usually parameterized)
              | GTBITS8
              | GTBITS16
              | GTBITS32
              | GTBITS64
              | GTSTRINGC
              | GTSTRINGFW  -- String (usually parameterized)
              | GTCHAR
              | GTINT
              | GTLIST [ GENTYPE ]
              | LIT Char    --Arguments to APP
              | INT Integer --Arguments to APP
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

randInteger :: MWC.GenIO -> IO Int
randInteger gen = RN.randInt 0 2147483647 gen

-- Actual value generation, outputs string/char list
mk_gen_char :: GENTYPE -> MWC.GenIO -> IO [Char]
mk_gen_char (GTCHAR) gen = (:[]) <$> randLetter gen
mk_gen_char (LIT c)  gen = return [c]               -- If we have a constant, produce it.
mk_gen_char (GTINT)  gen = show <$> randInteger gen
mk_gen_char (APP GTSTRINGC (LIT c)) gen = do
    len <- RN.randInt 1 listLimit gen
    str <- replicateM len (randLetter gen)
    return $ str ++ [c]
mk_gen_char (APP GTSTRINGFW (INT n)) gen =
    concat <$> replicateM (fromIntegral n) (mk_gen_char GTCHAR gen)
mk_gen_char (APP GTBITS8 (INT n)) gen = do
    when (n > 8 || n < 0) (error ("Bad Bits8 value: " ++ (show n)))
    r <- RN.randInt 0 (2 ^ n - 1) gen
    let n' = fromIntegral n
    let c = word8ToChr $ fromIntegral $ BTS.shiftL r (8 - n')
    return [c]
mk_gen_char (GTLIST ds) gen = do
    vals <- mapM (\x -> mk_gen_char x gen) ds
    return $ concat vals
mk_gen_char (CHOICE cs) gen = do
    rand <- RN.randElem cs gen
    mk_gen_char rand gen
mk_gen_char (x) _ = error $ "Unimplemented generation: " ++ (show x)  -- = (\x -> '_':[]) --if we have no idea what to do


mk_render_char :: PadsDecl -> MWC.GenIO -> IO String
mk_render_char pd gen = do
    print pd
    ds   <- gen_ast pd
    vals <- mapM (\x -> mk_gen_char x gen) ds
    return $ concat vals

findPadsAst name lst =
    case List.find (((== name) . fst )) lst
      of Just n  -> n
         Nothing -> error $ "PADS identifier " ++ (show name) ++ " not found"

res str= let
  ((x, (y, z)), s) = sTART_parseS str
  bad = numErrors y + length s
  in bad

generate :: [Char] -> IO String
generate padsID = do
    gen <- MWC.createSystemRandom
    mk_render_char (snd (findPadsAst padsID padsExp)) gen
-- example usage: generate "START" -- creates instance from START pads description
-- sTART_parseS generate "START"
