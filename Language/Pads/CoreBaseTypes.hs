{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, ScopedTypeVariables, 
             MultiParamTypeClasses, DeriveDataTypeable, TypeSynonymInstances, 
             FlexibleInstances #-}

{-
** *********************************************************************
*                                                                      *
*         (c)  Kathleen Fisher <kathleen.fisher@gmail.com>             *
*              John Launchbury <john.launchbury@gmail.com>             *
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
import qualified Data.ByteString as B  

import Language.Pads.LazyList 

import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Data.Data
import qualified Data.Map as M
import qualified Data.List as List
import Data.Word
import Data.Char as Char
import Data.Int

import Text.PrettyPrint.Mainland as PP   

import Control.Monad




-----------------------------------------------------------------

--type Char
type Char_md = Base_md

char_parseM :: PadsParser (Char, Base_md)
char_parseM  =
  handleEOF def "Char" $
  handleEOR def "Char" $ do
    c <- takeHeadP
    returnClean c

instance Pads Char Base_md where
  parsePP = char_parseM
  printFL = char_printFL

char_printFL :: (Char, Base_md) -> FList
char_printFL (c,bmd) = addString [c] 


-----------------------------------------------------------------

--type Int
type Int_md = Base_md

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

instance Pads Int Base_md where
  parsePP = int_parseM
  printFL = int_printFL

int_printFL :: (Int, Base_md) -> FList
int_printFL (i, bmd) = fshow i


-----------------------------------------------------------------

--type Double
type Double_md = Base_md

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

instance Pads Double Base_md where
  parsePP = double_parseM
  printFL = double_printFL

double_printFL :: (Double, Base_md) -> FList
double_printFL (d, bmd) = fshow d



-----------------------------------------------------------------

type Try a = a
type Try_md a_md = (Base_md, a_md)

try_parseM p = parseTry p
try_printFL p = printFL p


-----------------------------------------------------------------

type Digit = Int
type Digit_md = Base_md

digit_parseM :: PadsParser (Digit, Base_md)
digit_parseM  =
  handleEOF def "Pdigit" $
  handleEOR def "Pdigit" $ do
    c <- takeHeadP
    if isDigit c 
      then returnClean (digitToInt c)
      else returnError def (E.FoundWhenExpecting [c] "Digit")

digit_printFL :: (Digit, Base_md) -> FList
digit_printFL (i, bmd) = fshow i




-----------------------------------------------------------------

newtype Text = Text S.RawStream
  deriving (Eq, Show, Data, Typeable, Ord)
type Text_md = Base_md

text_parseM :: PadsParser (Text, Base_md)
text_parseM = do
  document <- getAllBinP
  returnClean (Text document)

instance Pretty Text where
  ppr (Text str) = text "ASCII"


instance Pads Text Base_md where
  parsePP = text_parseM
  printFL = text_printFL


text_printFL :: (Text, Base_md) -> FList
text_printFL (Text str, bmd) = addBString str


-----------------------------------------------------------------

newtype Binary = Binary S.RawStream
  deriving (Eq, Show, Data, Typeable, Ord)
type Binary_md = Base_md

binary_parseM :: PadsParser (Binary, Base_md)
binary_parseM = do
  document <- getAllBinP
  returnClean (Binary document)

instance Pretty Binary where
  ppr (Binary str) = text "Binary"

instance Pads Binary Base_md where
  parsePP = binary_parseM
  printFL = binary_printFL

binary_printFL :: (Binary, Base_md) -> FList
binary_printFL (Binary bstr, bmd) =  addBString bstr


-----------------------------------------------------------------

type StringC = String
type StringC_md = Base_md

stringC_parseM :: Char -> PadsParser (StringC, Base_md)
stringC_parseM c =
  handleEOF (stringC_def c) "StringC" $
  handleEOR (stringC_def c) "StringC" $ do
    str <- satisfy (\c'-> c /= c')
    returnClean str

stringC_def c = ""

stringC_printFL :: Char -> (StringC, Base_md) -> FList
stringC_printFL c (str, bmd) = addString str


-----------------------------------------------------------------

type StringFW = String
type StringFW_md = Base_md

stringFW_parseM :: Int -> PadsParser (StringFW, Base_md)
stringFW_parseM 0 = returnClean ""
stringFW_parseM n =
  handleEOF (stringFW_def n) "StringFW" $
  handleEOR (stringFW_def n) "StringFW" $ do
    str <- takeP n 
    if (length str) == n 
      then returnClean str
      else returnError (stringFW_def n) (E.Insufficient (length str) n)

stringFW_def n = take n (repeat 'X')

stringFW_printFL :: Int -> (StringFW, Base_md) -> FList
stringFW_printFL n (str, bmd)  = addString (take n str)


-----------------------------------------------------------------

type StringME = String
type StringME_md = Base_md

stringME_parseM :: RE -> PadsParser (StringME, Base_md)
stringME_parseM re = 
  handleEOF (stringME_def re) "StringME" $ do
    match <- regexMatchP re
    case match of 
      Just str -> returnClean str
      Nothing  -> returnError (stringME_def re) (E.RegexMatchFail (show re))

stringME_def (RE re) = "" -- should invert the re
stringME_def (REd re d) = d

stringME_printFL :: RE -> (StringME, Base_md) -> FList
stringME_printFL re (str, bmd) = addString str       
           -- We're not likely to check that str matches re


-----------------------------------------------------------------

type StringSE = String
type StringSE_md = Base_md

stringSE_parseM :: RE -> PadsParser (StringSE, Base_md)
stringSE_parseM re =
  checkEOF (stringSE_def re) "StringSE" $ 
  checkEOR (stringSE_def re) "StringSE" $ do
    match <- regexStopP re
    case match of 
      Just str -> returnClean str
      Nothing  -> returnError (stringSE_def re) (E.RegexMatchFail (show re))

stringSE_def (RE re) = "" -- should invert the re
stringSE_def (REd re d) = d

stringSE_printFL :: RE -> (StringSE, Base_md) -> FList
stringSE_printFL s (str, bmd) = addString str


-----------------------------------------------------------------

type StringP = String
type StringP_md = Base_md

stringP_parseM :: (Char -> Bool) -> PadsParser (StringP, Base_md)
stringP_parseM p =
  handleEOF (stringP_def p) "StringP" $ 
  handleEOR (stringP_def p) "StringP" $ do
    str <- satisfy p
    returnClean str

stringP_def _ = ""

stringP_printFL :: (Char -> Bool) -> (StringP, Base_md) -> FList
stringP_printFL p (str, bmd) = addString str

-----------------------------------------------------------------

type StringPESC = String
type StringPESC_md = Base_md

stringPESC_parseM :: (Bool, (Char, [Char])) -> PadsParser(StringPESC, Base_md)
stringPESC_parseM arg @ (endIfEOR, (escape, stops)) = 
 let (doEOF, doEOR) = if endIfEOR then (checkEOF, checkEOR) else (handleEOF, handleEOR)
 in
  doEOF "" "StringPESC" $
  doEOR "" "StringPESC" $ do 
    { c1 <- peekHeadP
    ; if c1 `elem` stops then 
         returnClean ""
      else if c1 == escape then do
         { takeHeadP
         ; doEOF [c1] "StringPESC" $
           doEOR [c1] "StringPESC" $ do
            { c2 <- takeHeadP
            ; if (c2 == escape) || (c2 `elem` stops) then do
                   { (rest, rest_md) <- stringPESC_parseM arg
                   ;  return (c2:rest, rest_md)
                   }
              else do 
                   { (rest, rest_md) <- stringPESC_parseM arg
                   ; return (c1:c2:rest, rest_md)
                   }
            }
         } else do 
            { c1 <- takeHeadP
            ; (rest, rest_md) <- stringPESC_parseM arg
            ; return (c1:rest, rest_md)
            }
    }



stringPESC_printFL :: (Bool, (Char, [Char])) -> (StringPESC, Base_md) -> FList
stringPESC_printFL (_, (escape, stops)) (str, bmd) = 
  let replace c = if c `elem` stops then escape : [c] else [c]
      newStr =  concat (map replace str)
  in addString newStr

-----------------------------------------------------------------


class LitParse a where
  litParse :: a -> PadsParser ((), Base_md)
  litPrint :: a -> FList

strLit_parseM :: String -> PadsParser ((), Base_md)
strLit_parseM s =
  handleEOF () s $ 
  handleEOR () s $ do
    match <- scanStrP s
    case match of
      Just []   -> returnClean ()
      Just junk -> returnError () (E.ExtraBeforeLiteral s)
      Nothing   -> returnError () (E.MissingLiteral     s)


instance LitParse Char where
  litParse = charLit_parseM
  litPrint = charLit_printFL

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


instance LitParse String where
  litParse = strLit_parseM
  litPrint = strLit_printFL


instance LitParse RE where
  litParse = reLit_parseM
  litPrint = reLit_printFL

reLit_parseM :: RE -> PadsParser ((), Base_md)
reLit_parseM re = do
  (match, md) <- stringME_parseM re
  if numErrors md == 0 
    then return ((), md) 
    else badReturn ((), md)



type EOF_md = Base_md

eof_parseM :: PadsParser ((), Base_md)
eof_parseM = do
  isEof <- isEOFP
  if isEof then returnClean ()
           else returnError () (E.ExtraBeforeLiteral "Eof")


type EOR_md = Base_md

eor_parseM :: PadsParser ((), Base_md)
eor_parseM = 
   handleEOF () "EOR" $ do
   isEor <- isEORP
   if isEor then doLineEnd
     else returnError () (E.LineError "Expecting EOR")







reLit_printFL :: RE -> FList
reLit_printFL (RE re)  = addString "--REGEXP LITERAL-- "
reLit_printFL (REd re def) = addString def

charLit_printFL :: Char ->  FList
charLit_printFL c  = addString [c] 

strLit_printFL :: String -> FList
strLit_printFL str  = addString str

eorLit_printFL :: FList
eorLit_printFL = printEOR

eofLit_printFL ::  FList
eofLit_printFL = printEOF

-----------------------------------------------------------------

newtype Void = Void ()
  deriving (Eq, Show, Data, Typeable, Ord)
type Void_md = Base_md

void_parseM :: PadsParser (Void, Base_md)
void_parseM = returnClean (Void ())

instance Pads Void Base_md where
  parsePP = void_parseM
  printFL = void_printFL

void_printFL :: a -> FList
void_printFL v = nil



pstrLit_printQ :: String -> FList 
pstrLit_printQ str = addString str

tuple_printQ :: (String, String, String) -> FList 
tuple_printQ (s1,s2,s3) = pstrLit_printQ s1 +++ pstrLit_printQ s2 +++ pstrLit_printQ s3

rtuple_printQ :: (String, String, String) -> FList 
rtuple_printQ ss = tuple_printQ ss +++ (addString ['\n'])

list_printQ :: [(String,String,String)] -> FList 
list_printQ [] =  nil
list_printQ (item:items) = rtuple_printQ item +++ list_printQ items

            
            
            




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

checkEOF val str p 
  = do { isEof <- isEOFP 
       ; if isEof then
           returnClean val
         else p}

checkEOR val str p 
  = do { isEor <- isEORP 
       ; if isEor then
           returnClean val
         else p}

----------------------------------
-- BINARY TYPES --
----------------------------------


type Bytes    = S.RawStream
type Bytes_md = Base_md

bytes_parseM :: Int -> PadsParser (Bytes,Bytes_md)
bytes_parseM n =
  handleEOF (def1 n) "Bytes" $
  handleEOR (def1 n) "Bytes" $ do
    bytes <- takeBytesP n
    if B.length bytes == n 
      then returnClean bytes
      else returnError (def1 n) (E.Insufficient (B.length bytes) n)

bytes_printFL :: Int -> (Bytes, Bytes_md) -> FList
bytes_printFL i (bs, bmd) = addBString bs

instance Pads1 Int Bytes Bytes_md where
  parsePP1 = bytes_parseM
  printFL1 = bytes_printFL


---- All the others can be derived from this: moved to BaseTypes.hs





{- Helper functions -}
mkStr c = "'" ++ [c] ++ "'"

