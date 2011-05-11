{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances #-}
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

module Language.Pads.BaseTypes where

import Language.Pads.Source
import Language.Pads.Errors 
import Language.Pads.Generic
import Language.Pads.MetaData
import Language.Pads.CoreBaseTypes
import Language.Pads.Quote
import Language.Pads.RegExp
import Language.Pads.LazyList
import Data.Time
import System.Locale

import qualified Data.Char as C
import qualified Data.List as L


[pads|
  type Line a = (a, EOR)
|]






[pads| type  Pstringln = Line (constrain x :: PstringSE <| RE "$" |> where <| True |>)           |]
[pads| type  Stringln =  Line (constrain x :: PstringSE <| RE "$" |> where <| True |>)           |]
[pads| type StringlnP (p :: String -> bool) = constrain s :: Stringln where <| p (toString s) |> |]


{-
[pads| type DateFSE (fmt :: String, se :: RE) = transform PstringSE se =>  UTCTime using <| (strToUTC fmt, utcToStr fmt) |> |]  

strToUTC :: Pos -> String -> (PstringSE, Base_md) -> (UTCTime, Base_md)
strToUTC pos fmt (PstringSE input, input_bmd) = 
  case parseTime defaultTimeLocale fmt input of 
       Nothing -> (gdef, mkErrBasePD  mergeBaseMDs [(TransformToDstFail "DateFSE" input " (conversion failed)") (Just pos), input_bmd])
       Just t  -> (t, input_bmd)

utcToStr :: String -> (UTCTime, Base_md) -> (PstringSE, Base_md) 
utcToStr fmt (utcTime, bmd) = (PstringSE (formatTime defaultTimeLocale fmt utcTime), bmd)

[pads| type TimeZoneSE (se :: RE) = transform PstringSE se =>  TimeZone using <| (strToTz, tzToStr) |> |]  

strToTz :: Pos -> (PstringSE, Base_md) -> (TimeZone, Base_md)
strToTz pos fmt (PstringSE input, input_bmd) = 
  case parseTime defaultTimeLocale "%z" input of 
       Nothing -> (gdef, mkErrBasePD  mergeBaseMDs [(TransformToDstFail "TimeZoneSE" input " (conversion failed)") (Just pos), input_bmd])
       Just t  -> (t, input_bmd)

tzToStr :: String -> (TimeZone, Base_md) -> (PstringSE, Base_md) 
tzToStr fmt (tz, bmd) = (PstringSE (h ++ ":" ++ m), bmd)
           where (h,m) = splitAt 3 (show tz)


[pads| type Phex32FW (size :: Int) = transform PstringFW size => Pint using <| (hexStr2Int,int2HexStr size) |> |]  

hexStr2Int :: Pos -> (PstringFW, Base_md) -> (Pint, Base_md)
hexStr2Int src_pos (PstringFW s,md) = if good then (Pint (intList2Int ints 0), md)
                                      else (0, mkErrBasePD  (TransformToDstFail "StrHex" s " (non-hex digit)") (Just src_pos))
  where
    hc2int c = if C.isHexDigit c then (C.digitToInt c,True) else (0,False)
    (ints,bools) = unzip (map hc2int s)
    good = (L.and bools) && (length ints > 0)
    intList2Int digits a = case digits of
        []     -> a
        (d:ds) -> intList2Int ds ((16 * a) + d)

int2HexStr :: Int -> (Pint, Base_md) -> (PstringFW, Base_md)
int2HexStr size (Pint x,md) = if (length result == size) && wasPos  then (PstringFW result, md)       
                              else if not wasPos then 
                                   (PstringFW (Prelude.take size result),    
                                    mkErrBasePD (TransformToSrcFail "StrHex" (show x) (" (Expected positive number)")) Nothing)
                              else (PstringFW (Prelude.take size result),
                                    mkErrBasePD (TransformToSrcFail "StrHex" (show x) (" (too big to fit in "++ (show size) ++" characters)")) Nothing)
  where
   cvt rest a = if rest < 16 then {- reverse $ -} (C.intToDigit rest) : a
                else cvt (rest `div` 16) (C.intToDigit (rest `mod` 16) : a)
   (wasPos,x') = if x < 0 then (False, -x) else (True, x)
   temp = cvt x' []
   padding = size - (length temp)
   stutter c n = if n <= 0 then [] else c : (stutter c (n-1))
   result = (stutter '0' padding) ++ temp



-}





