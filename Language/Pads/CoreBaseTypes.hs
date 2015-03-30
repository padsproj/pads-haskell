{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, ScopedTypeVariables, 
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
import Data.Maybe

import qualified Language.Pads.Source as S
import qualified Language.Pads.Errors as E
import qualified Data.ByteString as B  
import qualified Data.ByteString.Char8 as C

import Language.Pads.PadsPrinter 

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

type instance Meta Char = Base_md
instance Pads1 () Char Base_md where
  parsePP1 () = char_parseM
  printFL1 () = char_printFL

char_printFL :: PadsPrinter (Char, md)
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

type instance Meta Int = Base_md
instance Pads1 () Int Base_md where
  parsePP1 () = int_parseM
  printFL1 () = int_printFL

int_printFL :: PadsPrinter (Int, Base_md)
int_printFL (i, bmd) = fshow i

-----------------------------------------------------------------

--type Integer
type Integer_md = Base_md

integer_parseM :: PadsParser (Integer,Base_md)
integer_parseM =
  handleEOF def "Integer" $
  handleEOR def "Integer" $ do
    c <- peekHeadP 
    let isNeg = (c == '-')
    when isNeg (takeHeadP >> return ())
    digits <- satisfy Char.isDigit
    if not (null digits)
      then returnClean (toEnum $ digitListToInt isNeg digits)
      else returnError def (E.FoundWhenExpecting (mkStr c) "Integer")

type instance Meta Integer = Base_md
instance Pads1 () Integer Base_md where
  parsePP1 () = integer_parseM
  printFL1 () = integer_printFL

integer_printFL :: PadsPrinter (Integer, Base_md)
integer_printFL (i, bmd) = fshow i

-----------------------------------------------------------------

--type Float
type Float_md = Base_md

float_parseM :: PadsParser (Float,Base_md)
float_parseM =
  handleEOF def "Float" $
  handleEOR def "Float" $ do
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
      else returnError def (E.FoundWhenExpecting (mkStr c) "Float")

type instance Meta Float = Base_md
instance Pads1 () Float Base_md where
  parsePP1 () = float_parseM
  printFL1 () = float_printFL

float_printFL :: PadsPrinter (Float, Base_md)
float_printFL (d, bmd) = fshow d

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

type instance Meta Double = Base_md
instance Pads1 () Double Base_md where
  parsePP1 () = double_parseM
  printFL1 () = double_printFL

double_printFL :: PadsPrinter (Double, Base_md)
double_printFL (d, bmd) = fshow d



-----------------------------------------------------------------

-- tries to parse @a@ without consuming the input string
type Try a = a
type Try_md a_md = (Base_md, a_md)

try_parseM p = parseTry p
try_printFL :: PadsPrinter (a,a_md) -> PadsPrinter (Try a,Try_md a_md)
try_printFL p _ = printNothing


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

digit_printFL :: PadsPrinter (Digit, Base_md)
digit_printFL (i, bmd) = fshow i


-----------------------------------------------------------------

--type String
type String_md = Base_md

string_parseM :: PadsParser (String, Base_md)
string_parseM = do
  document <- getAllBinP
  returnClean $ C.unpack document

type instance Meta String = Base_md
instance Pads1 () String Base_md where
  parsePP1 () = string_parseM
  printFL1 () = string_printFL


string_printFL :: PadsPrinter (String, Base_md)
string_printFL (str, bmd) = addString str


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

type instance Meta Text = Base_md
instance Pads1 () Text Base_md where
  parsePP1 () = text_parseM
  printFL1 () = text_printFL


text_printFL :: PadsPrinter (Text, Base_md)
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

type instance Meta Binary = Base_md
instance Pads1 () Binary Base_md where
  parsePP1 () = binary_parseM
  printFL1 () = binary_printFL

binary_printFL :: PadsPrinter (Binary, Base_md)
binary_printFL (Binary bstr, bmd) = addBString bstr


-----------------------------------------------------------------

-- string with end character
type StringC = String
type StringC_md = Base_md

stringC_parseM :: Char -> PadsParser (StringC, Base_md)
stringC_parseM c =
  handleEOF (stringC_def c) "StringC" $
  handleEOR (stringC_def c) "StringC" $ do
    str <- satisfy (\c'-> c /= c')
    returnClean str

stringC_def c = ""

stringC_printFL :: Char -> PadsPrinter (StringC, Base_md) 
stringC_printFL c (str, bmd) = addString str


-----------------------------------------------------------------

-- string of fixed length
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

stringFW_def n = replicate n 'X'

stringFW_printFL :: Int -> PadsPrinter (StringFW, Base_md)
stringFW_printFL n (str, bmd)  = addString (take n str)

-----------------------------------------------------------------

-- string of variable length
type StringVW = String
type StringVW_md = Base_md

stringVW_parseM :: Int -> PadsParser (StringVW, Base_md)
stringVW_parseM 0 = returnClean ""
stringVW_parseM n =
  handleEOF (stringVW_def n) "StringVW" $
  handleEOR (stringVW_def n) "StringVW" $ do
    str <- takeP n 
    returnClean str

stringVW_def n = replicate n 'X'

stringVW_printFL :: Int -> PadsPrinter (StringVW, Base_md)
stringVW_printFL n (str, bmd)  = addString (take n str)

---- string of variable length (end if EOR)
--type StringVW = String
--type StringVW_md = Base_md
--
--stringVW_parseM :: (Bool,Int) -> PadsParser (StringVW, Base_md)
--stringVW_parseM (endIfEOR,0) = returnClean ""
--stringVW_parseM (endIfEOR,n) = do
--	let (doEOF, doEOR) = if endIfEOR then (checkEOF, checkEOR) else (handleEOF, handleEOR)
--	doEOF "" "StringVW" $ doEOR "" "StringVW" $ do
--		c1 <- takeHeadP
--		(rest, rest_md) <- stringVW_parseM (endIfEOR,pred n)
--		return (c1:rest, rest_md)
--
--stringVW_def (endIfEOR,n) = replicate n 'X'
--
--stringVW_printFL :: (Bool,Int) -> PadsPrinter (StringVW, Base_md)
--stringVW_printFL (endIfEOR,n) (str, bmd)  = addString (take n str)

-----------------------------------------------------------------

-----------------------------------------------------------------

-- string with matching expression
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

stringME_printFL :: RE -> PadsPrinter (StringME, Base_md)
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

stringSE_printFL :: RE -> PadsPrinter (StringSE, Base_md)
stringSE_printFL re (str, bmd) = addString str


-----------------------------------------------------------------

-- string with a predicate
type StringP = String
type StringP_md = Base_md

stringP_parseM :: (Char -> Bool) -> PadsParser (StringP, Base_md)
stringP_parseM p =
  handleEOF (stringP_def p) "StringP" $ 
  handleEOR (stringP_def p) "StringP" $ do
    str <- satisfy p
    returnClean str

stringP_def _ = ""

stringP_printFL :: (Char -> Bool) -> PadsPrinter (StringP, Base_md)
stringP_printFL p (str, bmd) = addString str

-----------------------------------------------------------------

-- string predicate with escape condition
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



stringPESC_printFL :: (Bool, (Char, [Char])) -> PadsPrinter (StringPESC, Base_md)
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

strLit_printFL :: String -> FList
strLit_printFL str = addString str

instance LitParse Char where
  litParse = charLit_parseM
  litPrint = charLit_printFL

charLit_parseM :: Char -> PadsParser ((),Base_md)
charLit_parseM c =
  handleEOF () (mkStr c) $
  handleEOR () (mkStr c) $ do
    c' <- takeHeadP 
    if c == c' then returnClean () else do
      foundIt <- scanP c
      returnError () (if foundIt 
                      then E.ExtraBeforeLiteral (mkStr c)
                      else E.MissingLiteral     (mkStr c)) 

charLit_printFL :: Char -> FList
charLit_printFL c = addString [c]

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

reLit_printFL :: RE -> FList
reLit_printFL (RE re) = addString "--REGEXP LITERAL-- "
reLit_printFL (REd re def) = addString def

type EOF = ()
type EOF_md = Base_md

eof_parseM :: PadsParser (EOF, Base_md)
eof_parseM = do
  isEof <- isEOFP
  if isEof then returnClean ()
           else returnError () (E.ExtraBeforeLiteral "Eof")


type EOR = ()
type EOR_md = Base_md

eor_parseM :: PadsParser (EOR, Base_md)
eor_parseM = 
   handleEOF () "EOR" $ do
   isEor <- isEORP
   if isEor then doLineEnd
     else returnError () (E.LineError "Expecting EOR")

eor_printFL :: (EOR,Base_md) -> FList
eor_printFL = const eorLit_printFL

eOR_printFL = eor_printFL

eof_printFL :: (EOF,Base_md) -> FList
eof_printFL = const eofLit_printFL

eOF_printFL = eof_printFL

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

instance Pads1 () Void Base_md where
  parsePP1 () = void_parseM
  printFL1 () = void_printFL

void_printFL :: PadsPrinter (Void,Base_md)
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

bytes_printFL :: Int -> PadsPrinter (Bytes, Bytes_md)
bytes_printFL n (bs, bmd) =
	addBString bs

type instance Meta Bytes = Bytes_md
instance Pads1 Int Bytes Bytes_md where
  parsePP1 = bytes_parseM
  printFL1 = bytes_printFL


---- All the others can be derived from this: moved to BaseTypes.hs





{- Helper functions -}
mkStr c = "'" ++ [c] ++ "'"
