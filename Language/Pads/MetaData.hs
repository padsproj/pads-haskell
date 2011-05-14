{-# LANGUAGE NamedFieldPuns, RecordWildCards, FlexibleInstances, DeriveDataTypeable #-}
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


module Language.Pads.MetaData where

import qualified Language.Pads.Errors as E
import qualified Language.Pads.Source as S
import Text.PrettyPrint.Mainland as PP

import Data.Data
import Data.List

{- Base type library support -}
data Base_md = Base_md { numErrors :: Int
                       , errInfo   :: Maybe E.ErrInfo
                        -- Need to add location information, etc.
                       }
   deriving (Typeable, Data, Eq, Ord)


{- Meta data type class -}
class Data md => PadsMD md where
  get_md_header :: md -> Base_md
  replace_md_header :: md -> Base_md -> md

instance PadsMD Base_md where
  get_md_header b = b
  replace_md_header old new = new

instance Data b => PadsMD (Base_md,b) where
  get_md_header (h,b) = h
  replace_md_header (h1,b) h2 = (h2,b)




instance Pretty Base_md where
  ppr = pprBaseMD

pprBaseMD Base_md {numErrors=num, errInfo = info} = text "Errors:" <+> PP.ppr num <+> 
                                                    case info of Nothing -> empty
                                                                 Just e -> PP.ppr e




cleanBasePD = Base_md {numErrors = 0, errInfo = Nothing }
mkErrBasePDfromLoc msg loc = Base_md {numErrors = 1, 
                               errInfo = Just (E.ErrInfo{msg=msg,position= Just (S.locToPos loc)}) }  

mkErrBasePD msg pos = Base_md {numErrors = 1, 
                               errInfo = Just (E.ErrInfo{msg=msg,position= pos}) }

shallowBaseMDs mds = case mds of 
                     [] -> cleanBasePD
                     otherwise ->  Data.List.foldl1 (\(Base_md {numErrors=num1,errInfo=i1}) (Base_md {numErrors=num2,errInfo=i2}) ->
                                                          (Base_md {numErrors=num1 + num2, errInfo= Nothing })) mds

mergeBaseMDs :: [Base_md] -> Base_md
mergeBaseMDs []  = cleanBasePD
mergeBaseMDs mds = foldl1 (\(Base_md {numErrors=num1,errInfo=i1}) (Base_md {numErrors=num2,errInfo=i2}) ->
                                                          (Base_md {numErrors=num1 + num2, errInfo= E.maybeMergeErrInfo i1 i2 })) mds

