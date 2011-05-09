{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}
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
*                                                                      *
************************************************************************
-}

module Language.Pads.Accum where
import Language.Pads.Padsc  hiding (empty)

import qualified Data.IntMap as IM
import qualified Data.Char as C

import Language.Haskell.TH

class Accum rep md where 
  data AccumRep rep md :: * 
  empty  :: AccumRep rep md 
  insert :: (rep,md) -> AccumRep rep md -> AccumRep rep md 
  insertMany :: [(rep,md)] -> AccumRep rep md -> AccumRep rep md 
  insertMany vals init = foldr insert init vals
  insertAll :: [(rep,md)] -> AccumRep rep md 
  insertAll vals = insertMany vals empty 

instance Accum Pint Base_md where
  data AccumRep Pint Base_md = AccumRepPint (IntAccum)
  empty                      = AccumRepPint emptyIntAccum
  insert (rep,pd) (AccumRepPint acc) = AccumRepPint (insertIntAccum (rep,pd) acc)

instance Accum Pchar Base_md where
  data AccumRep Pchar Base_md = AccumRepPchar (CharAccum)
  empty                       = AccumRepPchar emptyCharAccum
  insert (rep,pd) (AccumRepPchar acc) = AccumRepPchar (insertCharAccum (rep,pd) acc)

instance (Accum rep_a md_a, Accum rep_b md_b) => Accum (rep_a,rep_b) (Base_md, (md_a, md_b)) where
  data AccumRep (rep_a,rep_b) (Base_md, (md_a,md_b)) = AccumRepPair (AccumRep rep_a md_a, AccumRep rep_b md_b)
  empty                                               = AccumRepPair (empty, empty)
  insert ((rep_a,rep_b), (base_md, (md_a, md_b))) (AccumRepPair (acc_a, acc_b)) 
                        = AccumRepPair (insert (rep_a,md_a) acc_a, insert (rep_b, md_b) acc_b)

testInts = [ (Pint 0, cleanBasePD)
           , (Pint 1, cleanBasePD)
           , (Pint 2, cleanBasePD)
           , (Pint 3, cleanBasePD)
           , (Pint 4, cleanBasePD)
           , (Pint 0, mkErrBasePD (FoundWhenExpecting "Char" "Int") Nothing )
           ]

testInts_results = let AccumRepPint a = insertMany testInts empty in a

data IntAccum = IntAccum { numSeenInt :: Int,
                           numGoodSeenInt :: Int,
                           minSeenInt :: Int,
                           maxSeenInt :: Int, 
                           totSeenInt :: Int, 
                           histogramInt :: IM.IntMap Int }  -- for each int, count number of occurrences
  deriving (Eq, Show)

emptyIntAccum = IntAccum { numSeenInt = 0,
                           numGoodSeenInt = 0,
                           minSeenInt = maxBound,
                           maxSeenInt = minBound, 
                           totSeenInt = 0, 
                           histogramInt = IM.empty }

insertIntAccum :: (Pint, Base_md) -> IntAccum -> IntAccum
insertIntAccum (Pint i, md) orig = 
     if (numErrors md) == 0 
     then IntAccum
             { numSeenInt     = 1 + numSeenInt orig
             , numGoodSeenInt = 1 + numGoodSeenInt orig 
             , minSeenInt     = if i < (minSeenInt orig) then i else minSeenInt orig
             , maxSeenInt     = if i > (maxSeenInt orig) then i else maxSeenInt orig
             , totSeenInt     = i +  (totSeenInt orig) 
             , histogramInt   = IM.insertWith (\new-> \old -> old + 1) i 1 (histogramInt orig) 
             }
     else IntAccum
             { numSeenInt = 1 + numSeenInt orig
             , numGoodSeenInt = numGoodSeenInt orig
             , minSeenInt     = minSeenInt orig
             , maxSeenInt     = maxSeenInt orig
             , totSeenInt     = totSeenInt orig
             , histogramInt   = histogramInt orig
             }

testChrs = [ (Pchar 'a', cleanBasePD)
           , (Pchar 'b', cleanBasePD)
           , (Pchar 'e', cleanBasePD)
           , (Pchar ',', cleanBasePD)
           , (Pchar 'A', cleanBasePD)
           , (Pchar '\0', mkErrBasePD (FoundWhenExpecting "Int" "Char") Nothing )
           ]
testChrs_results = let AccumRepPchar a = insertMany testChrs empty
                    in a

data CharAccum = CharAccum { numSeenChar :: Int,
                             numGoodSeenChar :: Int,
                             minSeenChar :: Char,
                             maxSeenChar :: Char, 
                             histogramChar :: IM.IntMap Int }  -- for each char, count number of occurrences
  deriving (Eq, Show)

emptyCharAccum = CharAccum { numSeenChar = 0,
                             numGoodSeenChar = 0,
                             minSeenChar =  C.chr 255,
                             maxSeenChar = C.chr 0, 
                             histogramChar = IM.empty }

insertCharAccum :: (Pchar, Base_md) -> CharAccum -> CharAccum
insertCharAccum (Pchar c, md) orig = 
     if (numErrors md) == 0 
     then CharAccum
             { numSeenChar     = 1 + numSeenChar orig
             , numGoodSeenChar = 1 + numGoodSeenChar orig 
             , minSeenChar     = if c < (minSeenChar orig) then c else minSeenChar orig
             , maxSeenChar     = if c > (maxSeenChar orig) then c else maxSeenChar orig
             , histogramChar   = IM.insertWith (\new -> \old -> old + 1) (C.ord c) 1 (histogramChar orig) 
             }
     else CharAccum
             { numSeenChar = 1 + numSeenChar orig
             , numGoodSeenChar = numGoodSeenChar orig
             , minSeenChar     = minSeenChar orig
             , maxSeenChar     = maxSeenChar orig
             , histogramChar   = histogramChar orig
             }
               
instance Pretty (Q ()) where
  ppr arg = text "hello unit"

getInfo =  do r <- reify (mkName "Accum")
              (report True ("getInfo Accum" ++ (show r) ))



{-

things we would like to generically:
  (rep, md) -> XML
  (rep, md) -> XDTD
  (rep, md) --> pp rep  (String)
  (rep, md) --> pp md  (String)
  default: (rep, md) 
  random
  find : ((rep,md) -> bool) -> [(rep,md)] -> [(rep,md)]   -- polymorphic, already exists
-}