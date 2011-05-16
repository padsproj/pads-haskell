{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, ScopedTypeVariables, MultiParamTypeClasses, DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances #-}

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
import System.ByteOrder

import Language.Pads.LazyList 

import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Data.Data
import qualified Data.Map as M
import qualified Data.List as List
import Data.Word
import Data.Bits
import Char
import Data.Int

import Text.PrettyPrint.Mainland as PP   

import Monad


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
  ("Double",    (''Double, [])),
  ("Void",      (''Void, []))
 ]



baseTypesMap :: M.Map String (Name, [Name]) = M.fromList baseTypesList


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

type Text = String
type Text_md = Base_md

text_parseM :: PadsParser (Text, Base_md)
text_parseM = do
  document <- getAllP
  returnClean document

text_printFL :: (Text, Base_md) -> FList
text_printFL (str, bmd) = addString str


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

stringME_printFL :: RE -> (StringME, Base_md) -> FList
stringME_printFL re (str, bmd) = addString str       
           -- We're not likely to check that str matches re


-----------------------------------------------------------------

type StringSE = String
type StringSE_md = Base_md

stringSE_parseM :: RE -> PadsParser (StringSE, Base_md)
stringSE_parseM re =
  handleEOF (stringSE_def re) "StringSE" $ do
    match <- regexStopP re
    case match of 
      Just str -> returnClean str
      Nothing  -> returnError (stringSE_def re) (E.RegexMatchFail (show re))

stringSE_def (RE re) = "" -- should invert the re

stringSE_printFL :: RE -> (StringSE, Base_md) -> FList
stringSE_printFL s (str, bmd) = addString str



-----------------------------------------------------------------

type StringESC = String
type StringESC_md = Base_md

stringESC_parseM :: (Char, [Char]) -> PadsParser(StringESC, Base_md)
stringESC_parseM (escape, stops) = 
  handleEOF "" "StringESC" $
  handleEOR "" "StringESC" $ do 
    { c1 <- peekHeadP
    ; if c1 `elem` stops then 
         returnClean ""
      else if c1 == escape then do
         { takeHeadP
         ; c2 <- takeHeadP
         ; if (c2 == escape) || (c2 `elem` stops) then do
            { (rest, rest_md) <- stringESC_parseM (escape, stops) 
            ;  return (c2:rest, rest_md)
            }
           else do 
             { (rest, rest_md) <- stringESC_parseM (escape, stops) 
             ; return (c1:c2:rest, rest_md)
             }
         } else do 
            { c1 <- takeHeadP
            ; (rest, rest_md) <- stringESC_parseM (escape, stops) 
            ; return (c1:rest, rest_md)
            }
    }

stringESC_printFL :: (Char, [Char]) -> (StringESC, Base_md) -> FList
stringESC_printFL (escape, stops) (str, bmd) = 
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

----------------------------------
-- BINARY TYPES --
----------------------------------
-- type Int32 : 32-bit, signed integers; bytes assembled in order
type Int32_md = Base_md

int32_parseM :: PadsParser (Int32,Base_md)
int32_parseM =
  handleEOF def "Int32" $
  handleEOR def "Int32" $ do
    bytes <- takeBytesP 4
    if B.length bytes == 4 
      then returnClean (bytesToInt32 Native bytes)
      else returnError def (E.Insufficient (B.length bytes) 4)

instance Pads Int32 Base_md where
  parsePP = int32_parseM
  printFL = int32_printFL

int32_printFL :: (Int32, Base_md) -> FList
int32_printFL (i, bmd) = addBString (int32ToBytes Native i)


--type Int32sbh : signed byte high, 32-bit, signed integers
type Int32sbh_md = Base_md
type Int32sbh = Int32

int32sbh_parseM :: PadsParser (Int32,Base_md)
int32sbh_parseM =
  handleEOF def "Int32sbh" $
  handleEOR def "Int32sbh" $ do
    bytes <- takeBytesP 4
    if B.length bytes == 4 
      then returnClean (bytesToInt32 SBH bytes)
      else returnError def (E.Insufficient (B.length bytes) 4)

int32sbh_printFL :: (Int32, Base_md) -> FList
int32sbh_printFL (i, bmd) = addBString (int32ToBytes SBH i)

--type Int32sbl : signed byte low, 32-bit, signed integers
type Int32sbl_md = Base_md
type Int32sbl = Int32

int32sbl_parseM :: PadsParser (Int32,Base_md)
int32sbl_parseM =
  handleEOF def "Int32sbl" $
  handleEOR def "Int32sbl" $ do
    bytes <- takeBytesP 4
    if B.length bytes == 4 
      then returnClean (bytesToInt32 SBL bytes)
      else returnError def (E.Insufficient (B.length bytes) 4)


int32sbl_printFL :: (Int32, Base_md) -> FList
int32sbl_printFL (i, bmd) = addBString (int32ToBytes SBL i)

--type Word32 :  32-bit, unsigned integers, raw order
type Word32_md = Base_md

word32_parseM :: PadsParser (Word32,Base_md)
word32_parseM =
  handleEOF def "Word32" $
  handleEOR def "Word32" $ do
    bytes <- takeBytesP 4
    if B.length bytes == 4 
      then returnClean (bytesToWord32 Native bytes)
      else returnError def (E.Insufficient (B.length bytes) 4)

instance Pads Word32 Base_md where
  parsePP = word32_parseM
  printFL = word32_printFL

word32_printFL :: (Word32, Base_md) -> FList
word32_printFL (i, bmd) = addBString (word32ToBytes Native i)


--type Word32sbh : signed byte high, 32-bit, unsigned integers
type Word32sbh_md = Base_md
type Word32sbh = Word32

word32sbh_parseM :: PadsParser (Word32,Base_md)
word32sbh_parseM =
  handleEOF def "Word32sbh" $
  handleEOR def "Word32sbh" $ do
    bytes <- takeBytesP 4
    if B.length bytes == 4 
      then returnClean (bytesToWord32 SBH bytes)
      else returnError def (E.Insufficient (B.length bytes) 4)

word32sbh_printFL :: (Word32, Base_md) -> FList
word32sbh_printFL (i, bmd) = addBString (word32ToBytes SBH i)

--type Word32sbl : signed byte low, 32-bit, unsigned integers
type Word32sbl_md = Base_md
type Word32sbl = Word32

word32sbl_parseM :: PadsParser (Word32,Base_md)
word32sbl_parseM =
  handleEOF def "Word32sbl" $
  handleEOR def "Word32sbl" $ do
    bytes <- takeBytesP 4
    if B.length bytes == 4 
      then returnClean (bytesToWord32 SBL bytes)
      else returnError def (E.Insufficient (B.length bytes) 4)

word32sbl_printFL :: (Word32, Base_md) -> FList
word32sbl_printFL (i, bmd) = addBString (word32ToBytes SBL i)


----------------------------------



{- Helper functions -}
mkStr c = "'" ++ [c] ++ "'"


-- SIGNED-BYTE-HIGH FUNCTIONS --
-- The most significant digit is on the left (lowest address).
-- These functions manage the case where the wire representation
-- is signed-byte high, regardless of the endianness of the host
-- machine.
data Endian = SBH | SBL | Native

bytesToInt32 :: Endian -> B.ByteString -> Int32
bytesToInt32 endian = fromIntegral . (bytesToWord32 endian)

int32ToBytes :: Endian -> Int32 -> B.ByteString
int32ToBytes endian = (word32ToBytes endian) . fromIntegral



bytesToWord32 :: Endian -> B.ByteString -> Word32
bytesToWord32 endian b = 
  let  b0 :: Word32 = fromIntegral (b `B.index` 0)
       b1 :: Word32 = fromIntegral (b `B.index` 1)
       b2 :: Word32 = fromIntegral (b `B.index` 2)
       b3 :: Word32 = fromIntegral (b `B.index` 3)
  in
    case (endian, byteOrder) of
     (SBH, BigEndian)    ->   assembleWord32 (b3, b2, b1, b0)
     (SBH, LittleEndian) ->   assembleWord32 (b0, b1, b2, b3)
     (SBL, BigEndian)    ->   assembleWord32 (b0, b1, b2, b3)
     (SBL, LittleEndian) ->   assembleWord32 (b3, b2, b1, b0)
     (Native,  BigEndian)    ->   assembleWord32 (b0, b1, b2, b3)
     (Native, LittleEndian)  ->   assembleWord32 (b3, b2, b1, b0)


word32ToBytes :: Endian -> Word32 -> B.ByteString 
word32ToBytes endian word32 = 
  let w0 :: Word8 = fromIntegral (shiftR (word32 .&. 0xFF000000) 24)
      w1 :: Word8 = fromIntegral (shiftR (word32 .&. 0x00FF0000) 16)
      w2 :: Word8 = fromIntegral (shiftR (word32 .&. 0x0000FF00)  8)
      w3 :: Word8 = fromIntegral         (word32 .&. 0x000000FF)  
  in case (endian, byteOrder) of
     (SBH, BigEndian)    ->   B.pack [w3,w2,w1,w0]
     (SBH, LittleEndian) ->   B.pack [w0,w1,w2,w3]
     (SBL, BigEndian)    ->   B.pack [w0,w1,w2,w3]
     (SBL, LittleEndian) ->   B.pack [w3,w2,w1,w0]
     (Native, BigEndian) ->   B.pack [w0,w1,w2,w3]
     (Native, LittleEndian) ->   B.pack [w3,w2,w1,w0]
      

assembleWord32 :: (Word32, Word32, Word32, Word32) -> Word32
assembleWord32 (b0, b1, b2, b3) = 
  shift b0 24 .|. shift b1 16 .|. shift b2 8 .|. b3

