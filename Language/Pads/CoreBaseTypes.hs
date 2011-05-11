{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, ScopedTypeVariables, MultiParamTypeClasses, DeriveDataTypeable, TypeSynonymInstances #-}
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


module Language.Pads.CoreBaseTypes where

import Language.Pads.Generic
import Language.Pads.MetaData
import Language.Pads.PadsParser
import Language.Pads.RegExp

import qualified Language.Pads.Source as S
import qualified Language.Pads.Errors as E
import qualified Data.ByteString.Lazy.Char8 as B   -- abstraction for output data

import Language.Pads.LazyList 

import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import qualified Data.Map as M
import Data.Data
import qualified Data.List as List

import Text.PrettyPrint.Mainland as PP   

import Monad
import Char

{-  Base types 
    Pads type name, underlying representation type name, list of parameter types.
-}

baseTypesList = [
  ("Pint",      (''Int,     [])),
  ("Pchar",     (''Char,    [])),
  ("Pdigit",    (''Int,     [])),
  ("Ptext",     (''String,  [])),
  ("Pbinary",   (''S.RawStream,  [])),
  ("Pre",       (''String,  [''String])),
  ("Pstring",   (''String,  [''Char])),
  ("StringC",   (''String,  [''Char])),
  ("PstringFW", (''String,  [''Int])),
  ("PstringME", (''String,  [''RE])),
  ("PstringSE", (''String,  [''RE])),
  ("Int",       (''Int, [])),
  ("Char",      (''Char, [])),
  ("Double",    (''Double, []))
 ]



baseTypesMap :: M.Map String (Name, [Name]) = M.fromList baseTypesList

{- XXX: These declarations should be generated from table above . -}
{- XXX: Default values should be defined to use parameter, so for example, we can generate a string of the proper length. -}
newtype Pint = Pint Int
  deriving (Eq, Show, Data, Typeable, Num, Ord, Integral, Real, Enum)
newtype Pchar = Pchar Char
  deriving (Eq, Show, Data, Typeable, Ord)
newtype Pdigit = Pdigit Int
  deriving (Eq, Show, Data, Typeable, Num, Ord, Integral, Real, Enum)
newtype Ptext    = Ptext String
  deriving (Eq, Show, Data, Typeable, Ord)
newtype Pbinary  = Pbinary S.RawStream
  deriving (Eq, Show, Data, Typeable, Ord)

type Pint_md = Base_md
type Pchar_md = Base_md
type Pdigit_md = Base_md
type Ptext_md = Base_md
type Pbinary_md = Base_md
type Int_md = Base_md
type Double_md = Base_md
type Char_md = Base_md
type EOF_md = Base_md
type EOR_md = Base_md

instance Pretty Pchar where
  ppr (Pchar c) = text (show c)

instance Pretty Ptext where
  ppr (Ptext str) = string ("\"" ++ str++ "\"")

instance Pretty Pre where
  ppr (Pre s) = string s
              
instance Pretty Pbinary where
  ppr (Pbinary str) = text "Binary"

instance Pads Pint Base_md where
  parsePP = pint_parseM
  printFL = pint_printFL

instance Pads Pchar Base_md where
  parsePP = pchar_parseM
  printFL = pchar_printFL

instance Pads Pdigit Base_md where
  parsePP = pdigit_parseM
  printFL = pdigit_printFL

instance Pads Ptext Base_md where
  parsePP = ptext_parseM
  printFL = ptext_printFL

instance Pads Pbinary Base_md where
  parsePP = pbinary_parseM
  printFL = pbinary_printFL

instance Pads Int Base_md where
  parsePP = int_parseM
  printFL = int_printFL

instance Pads Double Base_md where
  parsePP = double_parseM
  printFL = double_printFL

instance Pads Char Base_md where
  parsePP = char_parseM
  printFL = char_printFL



newtype Pre = Pre String
  deriving (Eq, Data, Typeable, Ord)
newtype Pstring    = Pstring    String
  deriving (Eq, Show, Data, Typeable, Ord)
newtype StringC    = StringC    String
  deriving (Eq, Show, Data, Typeable, Ord)
newtype PstringFW = PstringFW String
  deriving (Eq, Show, Data, Typeable, Ord)
newtype PstringME = PstringME String
  deriving (Eq, Show, Data, Typeable, Ord)
newtype PstringSE = PstringSE String
  deriving (Eq, Show, Data, Typeable, Ord)

type Pre_md     = Base_md
type Pstring_md = Base_md
type StringC_md = Base_md
type PstringFW_md = Base_md
type PstringME_md = Base_md
type PstringSE_md = Base_md

instance Pads1 String Pre Base_md where 
  parsePP1 = pre_parseM 
  printFL1 = pre_printFL

instance Pads1 Char Pstring Base_md where 
  parsePP1 = pstring_parseM 
  printFL1 = pstring_printFL

instance Pads1 Char StringC Base_md where 
  parsePP1 = stringC_parseM 
  printFL1 = stringC_printFL

instance Pads1 Int PstringFW Base_md where 
  parsePP1 = pstringFW_parseM 
  printFL1 = pstringFW_printFL

instance Pads1 RE PstringME Base_md where
  parsePP1 = pstringME_parseM
  printFL1 = pstringME_printFL

instance Pads1 RE PstringSE Base_md where
  parsePP1 = pstringSE_parseM
  printFL1 = pstringSE_printFL

class ToString a where
  toString :: a -> String

instance ToString Pre where
  toString (Pre s) = s

instance ToString Pstring where
  toString (Pstring s) = s

instance ToString StringC where
  toString (StringC s) = s

instance ToString PstringFW where
  toString (PstringFW s) = s
 
instance ToString PstringME where
  toString (PstringME s) = s

instance ToString PstringSE where
  toString (PstringSE s) = s


----------------------------------

handleEOF val str p 
  = do { isEof <- isEOFP 
       ; if isEof then
           returnError val (E.FoundWhenExpecting "EOF" str) 
         else p}

handleEOR val str p 
  = do { isEor <- isEORP 
       ; if isEor then
           returnError val (E.FoundWhenExpecting "EOR" str)
         else p}

----------------------------------



pstringFW_parseM :: Int -> PadsParser (PstringFW, Base_md)
pstringFW_parseM 0 = returnClean (PstringFW "")
pstringFW_parseM n =
  handleEOF (def1 n) "PstringFW" $
  handleEOR (def1 n) "PstringFW" $ do
    str <- takeP n 
    if (length str) == n 
      then returnClean (PstringFW str)
      else returnError (def1 n) (E.Insufficient (length str) n)

pstringME_parseM :: RE -> PadsParser (PstringME, Base_md)
pstringME_parseM re = 
  handleEOF (def1 re) "PstringME" $ do
    match <- regexMatchP re
    case match of 
      Just str -> returnClean (PstringME str)
      Nothing  -> returnError (def1 re) (E.RegexMatchFail (show re))

pre_parseM :: String -> PadsParser (Pre, Base_md)
pre_parseM sre =
  handleEOF (def1 sre) "Pre" $ do
    match <- regexMatchP (RE sre)
    case match of 
      Just str -> returnClean (Pre str)
      Nothing  -> returnError (def1 sre) (E.RegexMatchFail sre)


pstringSE_parseM :: RE -> PadsParser (PstringSE, Base_md)
pstringSE_parseM re =
  handleEOF (def1 re) "PstringSE" $ do
    match <- regexStopP re
    case match of 
      Just str -> returnClean (PstringSE str)
      Nothing  -> returnError (def1 re) (E.RegexMatchFail (show re))

pstring_parseM :: Char -> PadsParser (Pstring, Base_md)
pstring_parseM c =
  handleEOF (def1 c) "Pstring" $
  handleEOR (def1 c) "Pstring" $ do
    str <- satisfy (\c'-> c /= c')
    returnClean (Pstring str)

stringC_parseM :: Char -> PadsParser (StringC, Base_md)
stringC_parseM c =
  handleEOF (def1 c) "StringC" $
  handleEOR (def1 c) "StringC" $ do
    str <- satisfy (\c'-> c /= c')
    returnClean (StringC str)

ptext_parseM :: PadsParser (Ptext, Base_md)
ptext_parseM = do
  document <- getAllP
  returnClean (Ptext document)

pbinary_parseM :: PadsParser (Pbinary, Base_md)
pbinary_parseM = do
  document <- getAllBinP
  returnClean (Pbinary document)


pchar_parseM :: PadsParser (Pchar, Base_md)
pchar_parseM  =
  handleEOF def "Pchar" $
  handleEOR def "Pchar" $ do
    c <- takeHeadP
    returnClean (Pchar c)

char_parseM :: PadsParser (Char, Base_md)
char_parseM  =
  handleEOF def "Pchar" $
  handleEOR def "Pchar" $ do
    c <- takeHeadP
    returnClean c

pdigit_parseM :: PadsParser (Pdigit, Base_md)
pdigit_parseM  =
  handleEOF def "Pdigit" $
  handleEOR def "Pdigit" $ do
    c <- takeHeadP
    if isDigit c 
      then returnClean (Pdigit (digitToInt c))
      else returnError def (E.FoundWhenExpecting [c] "Pdigit")


pint_parseM :: PadsParser (Pint,Base_md)
pint_parseM =
  handleEOF def "Pint" $
  handleEOR def "Pint" $ do
    c <- peekHeadP 
    let isNeg = (c == '-')
    when isNeg (takeHeadP >> return ())
    digits <- satisfy Char.isDigit
    if not (null digits)
      then returnClean (Pint $ digitListToInt isNeg digits)
      else returnError def (E.FoundWhenExpecting (mkStr c) "Pint")

int_parseM :: PadsParser (Int,Base_md)
int_parseM =
  handleEOF def "Int" $
  handleEOR def "Int" $ do
    c <- peekHeadP 
    let isNeg = (c == '-')
    when isNeg (takeHeadP >> return ())
    digits <- satisfy Char.isDigit
    if not (null digits)
      then returnClean (digitListToInt isNeg digits)
      else returnError def (E.FoundWhenExpecting (mkStr c) "Int")

double_parseM :: PadsParser (Double,Base_md)
double_parseM =
  handleEOF def "Double" $
  handleEOR def "Double" $ do
    -- Get leading sign
    c <- peekHeadP 
    let isNeg = (c == '-')
    when isNeg (takeHeadP >> return ())
    let sign = if isNeg then "-" else ""
    -- Get digits before any dot
    digits1 <- satisfy Char.isDigit
    -- Get optional dot
    d <- peekHeadP 
    let hasDot = (d == '.')
    when hasDot (takeHeadP >> return ())
    let dec = if hasDot then "." else ""
    -- Get digits after dot
    digits2 <- satisfy Char.isDigit
    -- Get optional exponent marker
    e <- peekHeadP 
    let hasExp = (e == 'e')
    when hasExp (takeHeadP >> return ())
    let exp = if hasExp then "e" else ""
    -- Get optional exponent sign
    es <- peekHeadP 
    let hasESign = (es == '-')
    when hasESign (takeHeadP >> return ())
    let expSign = if hasESign then "-" else ""
    -- Get digits in the exponent
    digits3 <- satisfy Char.isDigit
    -- As long as the double had digits
    if not (null digits1)
      then returnClean (read (sign ++digits1++dec++digits2++exp++expSign++digits3))
      else returnError def (E.FoundWhenExpecting (mkStr c) "Double")




preLit_parseM :: RE -> PadsParser ((), Base_md)
preLit_parseM re = do
  (match, md) <- pstringME_parseM re
  if numErrors md == 0 
    then return ((), md) 
    else badReturn ((), md)


charLit_parseM :: Char -> PadsParser ((), Base_md)
charLit_parseM c =
  handleEOF () (mkStr c) $
  handleEOR () (mkStr c) $ do
    c' <- takeHeadP 
    if c == c' then returnClean () else do
      foundIt <- scanP c
      returnError () (if foundIt 
                      then E.ExtraBeforeLiteral (mkStr c)
                      else E.MissingLiteral     (mkStr c)) 


strLit_parseM :: String -> PadsParser ((), Base_md)
strLit_parseM s =
  handleEOF () s $ 
  handleEOR () s $ do
    match <- scanStrP s
    case match of
      Just []   -> returnClean ()
      Just junk -> returnError () (E.ExtraBeforeLiteral s)
      Nothing   -> returnError () (E.MissingLiteral     s)

eor_parseM :: PadsParser ((), Base_md)
eor_parseM =
  handleEOF () "EOR" $ do
   isEor <- isEORP
   if isEor then doLineEnd
     else returnError () (E.LineError "Expecting EOR")

eof_parseM :: PadsParser ((), Base_md)
eof_parseM = do
  isEof <- isEOFP
  if isEof then returnClean ()
           else returnError () (E.ExtraBeforeLiteral "Eof")

pvoidLit_parseM :: PadsParser ((), Base_md)
pvoidLit_parseM = returnClean ()

{- Printing functions -}
{- We assume that the value satisfies the corresponding specification. -}

pstringFW_printFL :: Int -> (PstringFW, Base_md) -> FList
pstringFW_printFL n (PstringFW str, bmd)  = addString str        -- Should we check that str has length n?

pstringME_printFL :: RE -> (PstringME, Base_md) -> FList
pstringME_printFL re (PstringME str, bmd) = addString str        -- We're not likely to check that str matches re

pre_printFL :: String -> (Pre, Base_md) -> FList
pre_printFL s (Pre str, bmd) = addString str

pstringSE_printFL :: RE -> (PstringSE, Base_md) -> FList
pstringSE_printFL s (PstringSE str, bmd) = addString str

pstring_printFL :: Char -> (Pstring, Base_md) -> FList
pstring_printFL c (Pstring str, bmd) = addString str

stringC_printFL :: Char -> (StringC, Base_md) -> FList
stringC_printFL c (StringC str, bmd) = addString str

ptext_printFL :: (Ptext, Base_md) -> FList
ptext_printFL (Ptext str, bmd) = addString str

pbinary_printFL :: (Pbinary, Base_md) -> FList
pbinary_printFL (Pbinary bstr, bmd) =  addBString bstr

pchar_printFL :: (Pchar, Base_md) -> FList
pchar_printFL (Pchar c,bmd) = addString [c] 

char_printFL :: (Char, Base_md) -> FList
char_printFL (c,bmd) = addString [c] 

pdigit_printFL :: (Pdigit, Base_md) -> FList
pdigit_printFL (Pdigit i, bmd) = fshow i

pint_printFL :: (Pint, Base_md) -> FList
pint_printFL (Pint i, bmd) = fshow i

int_printFL :: (Int, Base_md) -> FList
int_printFL (i, bmd) = fshow i

double_printFL :: (Double, Base_md) -> FList
double_printFL (d, bmd) = fshow d

preLit_printFL :: RE -> FList
preLit_printFL (RE re)  = addString "--REGEXP LITERAL-- "
preLit_printFL (REd re def) = addString def

charLit_printFL :: Char ->  FList
charLit_printFL c  = addString [c] 

strLit_printFL :: String -> FList
strLit_printFL str  = addString str

eorLit_printFL :: FList
eorLit_printFL = printEOR

eofLit_printFL ::  FList
eofLit_printFL = printEOF

pvoidLit_printFL :: FList
pvoidLit_printFL = nil



pstrLit_printQ :: String -> FList 
pstrLit_printQ str = addString str

tuple_printQ :: (String, String, String) -> FList 
tuple_printQ (s1,s2,s3) = pstrLit_printQ s1 +++ pstrLit_printQ s2 +++ pstrLit_printQ s3

rtuple_printQ :: (String, String, String) -> FList 
rtuple_printQ ss = tuple_printQ ss +++ (addString ['\n'])

list_printQ :: [(String,String,String)] -> FList 
list_printQ [] =  nil
list_printQ (item:items) = rtuple_printQ item +++ list_printQ items

              
input = [("abc", "def", "ghi"),
         ("jkl", "mno", "pqr"),
         ("stu", "vwx", "yz0"),
         ("Kat", "hle", "en"),
         ("Fi",  "sh",  "er"),
         ("Sta", "nfo", "rd "),
         ("Uni", "ver", "sity")
         , undefined]

main = do
  printF (list_printQ input)



{- Helper functions -}
mkStr c = "'" ++ [c] ++ "'"

