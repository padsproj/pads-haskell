{-# LANGUAGE FlexibleContexts, DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK prune #-}
{-|
  Module      : Language.Pads.PadsPrinter
  Description : Lazy Pads printing monad
  Copyright   : (c) 2011
                Kathleen Fisher <kathleen.fisher@gmail.com>
                John Launchbury <john.launchbury@gmail.com>
  License     : MIT
  Maintainer  : Karl Cronburg <karl@cs.tufts.edu>
  Stability   : experimental

	This module provides a way to lazily append to a ByteString for the purpose
	of printing said ByteString.

-}
module Language.Pads.PadsPrinter where

import qualified Data.ByteString as B
import qualified Language.Pads.Source as S
import qualified Data.List as List
import Language.Pads.Errors
import Data.Typeable
import Language.Pads.MetaData
import Data.Data

{- Printing Monad -}

-- |
type PadsPrinter a = a -> FList

-- Lazy append-lists

-- |
type FList = B.ByteString -> B.ByteString

-- |
(+++) :: FList -> FList -> FList
p +++ q = p . q

-- |
nil :: FList
nil = id

-- |
concatFL :: [FList] -> FList
concatFL fs = foldr (+++) nil fs

-- |
printNothing :: FList
printNothing ws = ws

-- |
addBString :: B.ByteString -> FList
addBString bs = B.append bs

-- |
addString :: String -> FList
addString s = B.append (S.strToByteString s)

-- |
fshow :: Show a => a -> FList
fshow x = B.append (S.strToByteString (show x))

-- |
printEOR :: FList
printEOR = addString ['\n']

-- |
printEOF :: FList
printEOF = addString []

-- |
endRecord :: FList -> FList
endRecord fst = fst +++ printEOR

-- |
printF :: FList -> IO ()
printF q = Prelude.print (B.unpack (q B.empty))

--------------------------------------------------

printList' (reps, (_,mds)) printItem printSep printTerm = (concatFL (List.intersperse printSep (map printItem (zip reps $ mds ++ repeat myempty))) ) +++ printTerm

printList :: (Data r,Data m) => (PadsPrinter (r,m)) -> FList -> FList -> ([r], (Base_md,[m])) -> FList
printList printItem printSep printTerm (reps, (_,mds)) =
   (concatFL (List.intersperse printSep (map printItem (zip reps $ mds ++ repeat myempty))) )
   +++ printTerm

{-



type IntPair = (Int,"|",Int)

=====>

type IntPair    = (Int, Int)
type IntPair_md = (Base_md, (Base_md, Base_md))

intPair_parseM
       = let
           f_rep x1 x3 = (x1, x3)
           f_md x1 x2 x3
             = (mergeBaseMDs
                  [get_md_header x1, get_md_header x2, get_md_header x3],
                (x1, x3))
         in
           (((return (f_rep, f_md) =@= int_parseM) =@ strLit_parseM "|")
          =@=
            int_parseM)

intPair_parseS = parseStringInput intPair_parseM]

intPair_printFL (r,m)
  = case (r,m) of
      ((r1,r2),(_,(m1,m2)))
        -> int_PrintFL (r1,m1) +++
           addString "|" +++
           int_PrintFL (r2,m2)

-}
