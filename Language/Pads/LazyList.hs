module Language.Pads.LazyList where

{-
** *********************************************************************
*                                                                      *
*              This software is part of the pads package               *
*           Copyright (c) 2005-2011 AT&T Knowledge Ventures            *
*                      and is licensed under the                       *
*                        Common Public License                         *
*                      by AT&T Knowledge Ventures                      *
*                                                                      *
*                A copy of the License is available at                 *
*                    www.padsproj.org/License.html                     *
*                                                                      *
*  This program contains certain software code or other information    *
*  ("AT&T Software") proprietary to AT&T Corp. ("AT&T").  The AT&T     *
*  Software is provided to you "AS IS". YOU ASSUME TOTAL RESPONSIBILITY*
*  AND RISK FOR USE OF THE AT&T SOFTWARE. AT&T DOES NOT MAKE, AND      *
*  EXPRESSLY DISCLAIMS, ANY EXPRESS OR IMPLIED WARRANTIES OF ANY KIND  *
*  WHATSOEVER, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF*
*  MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE, WARRANTIES OF  *
*  TITLE OR NON-INFRINGEMENT.  (c) AT&T Corp.  All rights              *
*  reserved.  AT&T is a registered trademark of AT&T Corp.             *
*                                                                      *
*                   Network Services Research Center                   *
*                          AT&T Labs Research                          *
*                           Florham Park NJ                            *
*                                                                      *
*              Kathleen Fisher <kfisher@research.att.com>              *
*              John Launchbury <john@galois.com>                       *
*                                                                      *
************************************************************************
-}


import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List as List

-- Lazy append-lists 

type FList = B.ByteString -> B.ByteString

(+++) :: FList -> FList -> FList 
p +++ q = \ws -> p (q ws)

nil :: FList 
nil = \ws -> ws

concatFL :: [FList] -> FList
concatFL (f:fs) = f +++ (concatFL fs)
concatFL [] = nil

--cons :: a -> FList a -> FList a
--cons x q = \ws -> x : q ws

fshow :: Show a => a -> FList 
fshow x = \ws -> B.append (B.pack (show x)) ws

addString :: String -> FList 
addString s = \ws -> B.append (B.pack s)  ws

printEOR :: FList
printEOR = addString ['\n']

printEOF :: FList
printEOF = addString []

printNothing :: FList
printNothing ws = ws

endRecord :: FList -> FList
endRecord fst = fst +++ printEOR

addBString :: B.ByteString -> FList 
addBString bs = \ws -> B.append bs  ws

printF :: FList -> IO ()
printF q = Prelude.print (B.unpack (q B.empty))


printList (reps, (_,mds)) printItem printSep printTerm = 
   (concatFL (List.intersperse printSep (map printItem (zip reps mds))) ) +++ printTerm



