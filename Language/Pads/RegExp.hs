{-# LANGUAGE DeriveDataTypeable #-}
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

module Language.Pads.RegExp where
import Data.Data


{- Regular expression support -}
data RE = RE String | ReName String | REd String String
  deriving (Eq, Data, Typeable, Show)


-- generate re like [ab]
-- input: [a,b]
-- output RE "[ab]"
-- XXX: should escape ], ^, \, but underlying re library seems not to be able to handle these forms
charClass :: [Char] -> RE
charClass list = RE ("["++list++"]")

-- generate re like [ab]
-- input: [a,b]
-- output RE "[^a^b]"
-- XXX: should escape ], ^, \, but underlying re library seems not to be able to handle these forms
opCharClass :: [Char] -> RE
opCharClass list = RE ("[^" ++ list ++ "]")

chr :: Char -> RE
chr c = RE (c:[])

str :: String -> RE
str s = RE s

-- a | b
(.|.) :: RE -> RE -> RE
(RE l) .|. (RE r) = RE (l++['|']++r)

-- ab
(.&.) :: RE -> RE -> RE
(RE l) .&. (RE r) = RE (l++r)

-- kleene star
(.*.) :: RE -> RE
(.*.) (RE l) = RE (l++"*")

-- +
(.+.) :: RE -> RE
(.+.) (RE l) = RE (l++"+")

-- ?
(.?.) :: RE -> RE
(.?.) (RE l) = RE (l++"?")

dot :: RE
dot = RE "."

re_parens :: RE -> RE
re_parens (RE r) = RE ("("++r++")")

