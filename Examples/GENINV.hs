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


module Examples.GENINV where

import Language.Pads.Padsc
import Examples.GEN


import qualified Language.Haskell.TH as TH

--import Language.Pads.Testing
import Data.Word
import Data.Maybe
--import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString as B
import qualified Data.Word as W
import qualified Data.List as DL
import qualified Data.Bits as BTS
import qualified Examples.Rand as RN

-- Bring into scope our pads ASTS.
$(make_pads_declarations $ map snd padsExp)
envs = padsExp


gen_ast (PadsDeclData id xs args padsdatas e) = gen_padsdatas padsdatas --  String [String] (Maybe Pat) PadsData [QString]
gen_ast (_) = error "nope_ast"


gen_padsdatas (PUnion branches) = 
  let 
    bs = (map gen_branches branches) --generate for each branch
    gs = map GTLIST bs               --make each branch a list
  in  
    [CHOICE gs] --embed in a CHOICE type
gen_padsdatas (_) = error "nope_padsdatas"


gen_branches (BRecord id fs exp) =  map gen_fieldinfo fs
gen_branches (BConstr id fs e) =    map gen_constarg fs

gen_fieldinfo ((id, constarg, exp)) = gen_constarg constarg

gen_constarg  ((strict, padsty)) =  gen_padsty padsty

listLimit = 20 --we limit how long generated lists can be.

-- This function actually takes apart padsty's

gen_padsty (PTycon xs)          = gen_base (xs !! 0)                     -- when would this be a list of more than one element?
                                                                         -- get back our base types 
gen_padsty (PApp xs (Just e))   = APP (gen_padsty (xs !! 0)) (gen_lit e) -- when would this be a list of more than one element?
                                                                         -- these are the application of an argument to a typeconstructor
                                                                         -- which take an argument
gen_padsty (PExpression exp)    = gen_lit exp -- expressions wrap literals, constants we need to produce
gen_padsty (PList pty delim term) = let       -- lists are things we need to generate several of, possibly with a delimiter
  t = gen_padsty pty
  n = (fromIntegral ((RN.randInt 1) listLimit)::Int)
  in
   case delim of 
    Just del -> let 
          d = gen_padsty del
         in 
                --mix in the delimiters
                GTLIST (DL.intersperse d (replicate n t))
    Nothing ->  GTLIST (replicate n t)
gen_padsty (_)                  = error "nope_padsty"


-- step out into template hasel to get the literal value we need and turn it into a GENTYPE
gen_lit (TH.LitE (TH.CharL x))    = (LIT x)
gen_lit (TH.LitE (TH.StringL xs)) = (GTLIST (map LIT xs) ) -- treat string as a list of chars
gen_lit (TH.LitE (TH.IntegerL x)) = (INT x) 



data GENTYPE =  GTBITFIELD  --Bitfield (usually parameterized)
              | GTBITS8
              | GTBITS16
              | GTBITS32
              | GTBITS64
              | GTSTRINGFW  -- String (usually parameterized)
              | GTCHAR      -- Make a random char
              | GTINT       -- Make a random int
              | GTLIST [ GENTYPE ] 
              | LIT Char    --Arguments to APP
              | INT Integer --Arguments to APP
              | APP GENTYPE GENTYPE
              | CHOICE [ GENTYPE]
  deriving (Show)

gen_base "Char"      = GTCHAR
gen_base "Int"       = GTINT
gen_base "StringFW"  = GTSTRINGFW
gen_base "BitField"  = GTBITFIELD
gen_base "Bits8"     = GTBITS8
gen_base "Bits16"    = GTBITS16
gen_base "Bits32"    = GTBITS32
gen_base "Bits64"    = GTBITS64
-- this will be replaced with something which actually can find all the ast's in a single location
gen_base x = (gen_lookup x envs)

-- If we couldn't find a base type, maybe it's another PADS type... try looking it up.
gen_lookup s ( (s1,b):xs ) | s == s1 = GTLIST(gen_ast b)
gen_lookup s ( (x,b):xs ) = gen_lookup s xs
gen_lookup s [] = error ("could not find ast_" ++ s)


--for the sake of generating we just choose one of these
randLetter x  = (RN.randElem ['X','Y','Z']:[])

--we limit our integers
randInteger x = ((RN.randInt 0) 99999999)

--convert an individual GENTYPE to a function which when invoked returns a string
--everything returns a string
mk_gen_char :: Num t => GENTYPE -> t -> [Char]
mk_gen_char (GTCHAR) = (\x ->  (randLetter 1))            --using some simple values (randLetter) to limit it
mk_gen_char (LIT c) = (\x ->  c:[])                       --If we have a constant, produce it.
mk_gen_char (GTINT) = (\x ->  show (randInteger 1))       --Make an integer
mk_gen_char (APP GTSTRINGFW (INT 1)) = mk_gen_char GTCHAR --string base case
mk_gen_char (APP GTSTRINGFW (INT n)) = let                --string inductive case
  head = ((mk_gen_char GTCHAR) 1) 
  tail = ((mk_gen_char (APP GTSTRINGFW (INT (n-1)))) 1) 
  in
  (\x -> head ++ tail)
mk_gen_char (GTLIST ds) = let
  fs = map mk_gen_char ds         -- create functions
  vs = map (\x -> (x 1)) fs       -- invoke functions getting back strings
  ss = foldr (\x y -> x++y) "" vs -- join strings
 in 
  (\x -> ss)
mk_gen_char (CHOICE cs) = mk_gen_char (RN.randElem cs)
mk_gen_char (_)    = error "unimplemented"  -- = (\x -> '_':[]) --if we have no idea what to do


--render a pads description to a random character string
mk_render_char pid = 
  let 
    ds = gen_ast pid                -- get the GENTYPE descriptions
    fs = map mk_gen_char ds         -- convert GENTYPES to functions which return strings
    vs = map (\x -> (x 1)) fs       -- invoke them
    ss = foldr (\x y -> x++y) "" vs -- join the resulting strings
  in
    ss


findPadsAst name lst = fromJust $ DL.find (((==name) . fst )) lst

res str= let
  ((x, (y, z)), s) = sTART_parseS str
  bad = numErrors y + length s
  in bad


generate padsid = mk_render_char (snd (findPadsAst padsid padsExp))
-- example usage: generate "START" -- creates instance from START pads description
-- sTART_parseS generate "START"