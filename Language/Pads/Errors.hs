{-# LANGUAGE NamedFieldPuns, DeriveDataTypeable #-}

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


module Language.Pads.Errors where
import Text.PrettyPrint.Mainland as PP
import qualified Language.Pads.Source as S
import Data.Data

data ErrMsg = 
   FoundWhenExpecting String String
 | MissingLiteral String
 | ExtraBeforeLiteral String
 | LineError String
 | Insufficient Int Int
 | RegexMatchFail String
 | TransformToDstFail String String String
 | TransformToSrcFail String String String
 | FUnderlyingTypedefFail
 | FPredicateFailure
 | ExtraStuffBeforeTy String String
 | FileError String String
   deriving (Typeable, Data, Eq, Ord)

{- XXX-KSF: fix pretty printing to use pretty printing combinators rather than string ++ -}
instance Pretty ErrMsg where
  ppr (FoundWhenExpecting str1 str2) = text ("Encountered " ++ str1 ++ " when expecting " ++ str2 ++ ".")
  ppr (MissingLiteral s)     = text ("Missing Literal: " ++ s ++ ".")
  ppr (ExtraBeforeLiteral s) = text ("Extra bytes before literal: " ++ s ++ ".")
  ppr (ExtraStuffBeforeTy junk ty) = text ("Extra bytes: " ++ junk ++ " before " ++ ty ++ ".")
  ppr (Insufficient found expected) = text("Found " ++ (show found) ++ " bytes when looking for " ++ (show expected) ++ "bytes.")
  ppr (RegexMatchFail s) = text ("Failed to match regular expression: " ++ s ++ ".")
  ppr (TransformToDstFail s1 s2 s3) = text ("Parsing transform " ++ s1 ++ " failed on input: " ++ s2 ++ s3)
  ppr (TransformToSrcFail s1 s2 s3) = text ("Printing transform "++ s1 ++ " failed on input: " ++ s2 ++ s3)
  ppr (LineError s)        = text s
  ppr FUnderlyingTypedefFail  = text "Forest predicate is true, but underlying type had an error."
  ppr FPredicateFailure       = text "Forest predicate is false."
  ppr (FileError err file) = text ("Problem with file: " ++ file ++ "("++ err ++ ").")

data ErrInfo = ErrInfo { msg      :: ErrMsg,
                         position :: Maybe S.Pos }
   deriving (Typeable, Data, Eq, Ord)

instance Pretty ErrInfo where
  ppr (ErrInfo {msg,position}) = PP.ppr msg <+> 
       case position of 
         Nothing -> empty
         Just pos -> (text "at:") <+>  PP.ppr pos



mergeErrInfo (ErrInfo{msg=msg1, position=position1}) (ErrInfo{msg=msg2, position=position2}) = 
             (ErrInfo{msg=msg1, position=position1})


maybeMergeErrInfo m1 m2 = case (m1,m2) of 
          (Nothing,Nothing) -> Nothing 
          (Just p, Nothing) -> Just p     
          (Nothing, Just p) -> Just p
          (Just p1, Just p2) -> Just (mergeErrInfo p1 p2)

