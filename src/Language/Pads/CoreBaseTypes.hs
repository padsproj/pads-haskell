{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, ScopedTypeVariables,
             MultiParamTypeClasses, DeriveDataTypeable, TypeSynonymInstances,
             FlexibleInstances, UndecidableInstances, DeriveLift #-}
{-# OPTIONS_HADDOCK prune #-}
{-|
  Module      : Language.Pads.CoreBaseTypes
  Description : Core Pads base types with parsers
  Copyright   : (c) 2011
                Kathleen Fisher <kathleen.fisher@gmail.com>
                John Launchbury <john.launchbury@gmail.com>
  License     : MIT
  Maintainer  : Karl Cronburg <karl@cs.tufts.edu>
  Stability   : experimental

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
import           Data.ByteString.Internal
import qualified Data.ByteString.Char8 as C

import Language.Pads.PadsPrinter
import Language.Pads.Generation

import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Data.Data
import qualified Data.Map as M
import qualified Data.List as List
import Data.Word
import Data.Char as Char
import Data.Int
import Data.Bits

import Text.PrettyPrint.Mainland as PP
import Text.PrettyPrint.Mainland.Class

import Control.Monad
import Control.Monad.Reader
import System.IO.Unsafe (unsafePerformIO)


-- | Metadata type for a PADS Char
type Char_md = Base_md

-- | Monadic parser for a PADS Char
char_parseM :: PadsParser (Char, Base_md)
char_parseM  =
  handleEOF def "Char" $
  handleEOR def "Char" $ do
    c <- takeHeadP
    returnClean c

-- | Default value inserted by the parser for a PADS Char
char_def :: Char
char_def = 'X'

type instance PadsArg Char = ()
type instance Meta Char = Base_md
instance Pads1 () Char Base_md where
  parsePP1 () = char_parseM
  printFL1 () = char_printFL
  def1 () = char_def

char_printFL :: PadsPrinter (Char, md)
char_printFL (c,bmd) = addString [c]

char_genM :: PadsGen Char
char_genM = randLetter

char_serialize :: Char -> CList
char_serialize c = toCL [CharChunk c]

---------------------------------------------

type BitBool = Bool
type BitBool_md = Base_md

bitBool_parseM :: PadsParser (BitBool, Base_md)
bitBool_parseM =
    handleEOF False "BitBool" $
    handleEOR False "BitBool" $ do
        b <- takeBits8P 1
        returnClean (b == 1)

bitBool_def = False

bitBool_printFL :: PadsPrinter (BitBool, md)
bitBool_printFL (bb,bbmd) = fshow bb

bitBool_genM :: PadsGen BitBool
bitBool_genM = randElem [False,True]

bitBool_serialize :: BitBool -> CList
bitBool_serialize b = toCL [BinaryChunk ((fromIntegral . fromEnum) b) 1]

-- type instance PadsArg Bool = ()
-- type instance Meta Bool = Base_md
-- instance Pads1 () Bool Base_md where
--     parsePP1 () = bitBool_parseM
--     printFL1 () = bitBool_printFL
--     def1 () = bitBool_def

-----------------------------------------------------------------

type BitField = Integer
type BitField_md = Base_md

bitField_parseM :: Integral a => a -> PadsParser (BitField, Base_md)
bitField_parseM x =
    if   x < 0
    then returnError def (E.BitWidthError 0 (fromIntegral x))
    else handleEOF 0 "BitField" $
         handleEOR 0 "BitField" $ do
             b <- takeBitsP x
             returnClean b

bitField_def :: a -> BitField
bitField_def _ = 0

bitField_printFL :: Integral a => a -> PadsPrinter (BitField, md)
bitField_printFL _ (x, xmd) = fshow x

bitField_genM :: Integral a => a -> PadsGen BitField
bitField_genM x = randIntegerBound (2^x - 1)

bitField_serialize :: Integral a => a -> BitField -> CList
bitField_serialize b v = toCL [BinaryChunk v (fromIntegral b)]


type Bits8 = Word8
type Bits8_md = Base_md

bits8_parseM :: Integral a => a -> PadsParser (Bits8, Base_md)
bits8_parseM x =
    if   x < 1 || x > 8
    then returnError 0 (E.BitWidthError 8 (fromIntegral x))
    else handleEOF 0 "Bits8" $
         handleEOR 0 "Bits8" $ do
             b <- takeBits8P x
             returnClean b

bits8_def :: a -> Bits8
bits8_def _ = 0

bits8_printFL  :: Integral a => a -> PadsPrinter (Bits8, md)
bits8_printFL  _ (x, xmd) = fshow x

bits8_genM :: Integral a => a -> PadsGen Bits8
bits8_genM x = randNumBound (2^x - 1)

bits8_serialize :: Int -> Bits8 -> CList
bits8_serialize b v = toCL [BinaryChunk (fromIntegral v) b]


type Bits16 = Word16
type Bits16_md = Base_md

bits16_parseM :: Integral a => a -> PadsParser (Bits16, Base_md)
bits16_parseM x =
    if   x < 1 || x > 16
    then returnError 0 (E.BitWidthError 16 (fromIntegral x))
    else handleEOF 0 "Bits16" $
         handleEOR 0 "Bits16" $ do
             b <- takeBits16P x
             returnClean b

bits16_def :: a -> Bits16
bits16_def _ = 0

bits16_printFL :: Integral a => a -> PadsPrinter (Bits16, md)
bits16_printFL _ (x, xmd) = fshow x

bits16_genM :: Integral a => a -> PadsGen Bits16
bits16_genM x = randNumBound (2^x - 1)

bits16_serialize :: Int -> Bits16 -> CList
bits16_serialize b v = toCL [BinaryChunk (fromIntegral v) b]


type Bits32 = Word32
type Bits32_md = Base_md

bits32_parseM :: Integral a => a -> PadsParser (Bits32, Base_md)
bits32_parseM x =
    if   x < 1 || x > 32
    then returnError 0 (E.BitWidthError 32 (fromIntegral x))
    else handleEOF 0 "Bits32" $
         handleEOR 0 "Bits32" $ do
             b <- takeBits32P x
             returnClean b

bits32_def :: a -> Bits32
bits32_def _ = 0

bits32_printFL :: Integral a => a -> PadsPrinter (Bits32, md)
bits32_printFL _ (x, xmd) = fshow x

bits32_genM :: Integral a => a -> PadsGen Bits32
bits32_genM x = randNumBound (2^x - 1)

bits32_serialize :: Int -> Bits32 -> CList
bits32_serialize b v = toCL [BinaryChunk (fromIntegral v) b]


type Bits64 = Word64
type Bits64_md = Base_md

bits64_parseM :: Integral a => a -> PadsParser (Bits64, Base_md)
bits64_parseM x =
    if   x < 1 || x > 64
    then returnError 0 (E.BitWidthError 64 (fromIntegral x))
    else handleEOF 0 "Bits64" $
         handleEOR 0 "Bits64" $ do
             b <- takeBits64P x
             returnClean b

bits64_def :: a -> Bits64
bits64_def _ = 0

bits64_printFL :: Integral a => a -> PadsPrinter (Bits64, md)
bits64_printFL _ (x, xmd) = fshow x

bits64_genM :: Integral a => a -> PadsGen Bits64
bits64_genM x = randNumBound (2^x - 1)

bits64_serialize :: Int -> Bits64 -> CList
bits64_serialize b v = toCL [BinaryChunk (fromIntegral v) b]

-----------------------------------------------------------------

--type Int
type Int_md = Base_md

-- | Monadic parser for a PADS Int
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

-- | Default value inserted by the parser for a PADS Int
int_def :: Int
int_def = 0

type instance PadsArg Int = ()
type instance Meta Int = Base_md
instance Pads1 () Int Base_md where
  parsePP1 () = int_parseM
  printFL1 () = int_printFL
  def1 () = int_def

int_printFL :: PadsPrinter (Int, Base_md)
int_printFL (i, bmd) = fshow i

int_genM :: PadsGen Int
int_genM = randNum

int_serialize :: Int -> CList
int_serialize i = toCL $ map CharChunk $ show i

-----------------------------------------------------------------

--type Integer
type Integer_md = Base_md

-- | Monadic parser for a PADS Integer
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

-- | Default value inserted by the parser for a PADS Integer
integer_def :: Integer
integer_def = 0

type instance PadsArg Integer = ()
type instance Meta Integer = Base_md
instance Pads1 () Integer Base_md where
  parsePP1 () = integer_parseM
  printFL1 () = integer_printFL
  def1 () = integer_def

integer_printFL :: PadsPrinter (Integer, Base_md)
integer_printFL (i, bmd) = fshow i

integer_genM :: PadsGen Integer
integer_genM = randInteger

integer_serialize :: Integer -> CList
integer_serialize i = toCL $ map CharChunk $ show i

-----------------------------------------------------------------

--type Float
type Float_md = Base_md

-- | Monadic parser for a PADS Float, e.g. "-3.1415"
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

-- | Default value inserted by the parser for a PADS Float
float_def :: Float
float_def = 0

type instance PadsArg Float = ()
type instance Meta Float = Base_md
instance Pads1 () Float Base_md where
  parsePP1 () = float_parseM
  printFL1 () = float_printFL
  def1 () = float_def

float_printFL :: PadsPrinter (Float, Base_md)
float_printFL (d, bmd) = fshow d

float_genM :: PadsGen Float
float_genM = randNum

float_serialize :: Float -> CList
float_serialize f = toCL $ map CharChunk $ show f

-----------------------------------------------------------------

--type Double
type Double_md = Base_md

-- | Monadic parser for a textual PADS Double, e.g. "-3.1415"
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

-- | Default value inserted by the parser for a PADS Float
double_def :: Double
double_def = 0

type instance PadsArg Double = ()
type instance Meta Double = Base_md
instance Pads1 () Double Base_md where
  parsePP1 () = double_parseM
  printFL1 () = double_printFL
  def1 () = 0

double_printFL :: PadsPrinter (Double, Base_md)
double_printFL (d, bmd) = fshow d

double_genM :: PadsGen Double
double_genM = randNum

double_serialize :: Double -> CList
double_serialize d = toCL $ map CharChunk $ show d

-----------------------------------------------------------------

-- tries to parse @a@ without consuming the input string
type Try a = a
type Try_md a_md = (Base_md, a_md)

try_parseM :: PadsMD md => PadsParser (rep,md) -> PadsParser (Try rep, Try_md md)
try_parseM p = do
  (rep,md) <- parseTry p
  return (rep, (cleanBasePD, md))

try_printFL :: PadsPrinter (a,a_md) -> PadsPrinter (Try a,Try_md a_md)
try_printFL p _ = printNothing

try_def :: a -> Try a
try_def d = d

try_genM :: PadsGen a -> PadsGen (Try a)
try_genM g = g >>= return

--try_serialize :: Try a -> CList
try_serialize = id


-----------------------------------------------------------------

type Digit = Int
type Digit_md = Base_md

-- | Monadic parser for a PADS Digit according to @'isDigit'@
digit_parseM :: PadsParser (Digit, Base_md)
digit_parseM  =
  handleEOF def "Pdigit" $
  handleEOR def "Pdigit" $ do
    c <- takeHeadP
    if isDigit c
      then returnClean (digitToInt c)
      else returnError def (E.FoundWhenExpecting [c] "Digit")

-- | Default value inserted by the parser for a PADS Digit
digit_def :: Digit
digit_def = 0

digit_printFL :: PadsPrinter (Digit, Base_md)
digit_printFL (i, bmd) = fshow i

digit_genM :: PadsGen Digit
digit_genM = randNumBound 9

digit_serialize :: Digit -> CList
digit_serialize d = toCL $ map CharChunk $ show d


-----------------------------------------------------------------

--type String
type String_md = Base_md

string_parseM :: PadsParser (String, Base_md)
string_parseM = do
  document <- getAllBinP
  returnClean $ C.unpack document

-- | Default value inserted by the parser for a PADS String
string_def = ""

type instance PadsArg String = ()
type instance Meta String = Base_md
instance Pads1 () String Base_md where
  parsePP1 () = string_parseM
  printFL1 () = string_printFL
  def1 () = string_def

string_printFL :: PadsPrinter (String, Base_md)
string_printFL (str, bmd) = addString str

string_genM :: PadsGen String
string_genM = stringVW_genM 100

string_serialize :: String -> CList
string_serialize s = toCL $ map CharChunk s

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

text_def :: Text
text_def = Text $ B.pack []

type instance PadsArg Text = ()
type instance Meta Text = Base_md
instance Pads1 () Text Base_md where
  parsePP1 () = text_parseM
  printFL1 () = text_printFL
  def1 () = text_def

text_printFL :: PadsPrinter (Text, Base_md)
text_printFL (Text str, bmd) = addBString str

text_genM :: PadsGen Text
text_genM = Text <$> B.pack <$> (map S.chrToWord8) <$> stringVW_genM 500

text_serialize :: Text -> CList
text_serialize (Text b) = toCL $ ((map (CharChunk . S.word8ToChr)) . B.unpack) b

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

binary_def :: Binary
binary_def = Binary $ B.pack []

type instance PadsArg Binary = ()
type instance Meta Binary = Base_md
instance Pads1 () Binary Base_md where
  parsePP1 () = binary_parseM
  printFL1 () = binary_printFL
  def1 () = binary_def

binary_printFL :: PadsPrinter (Binary, Base_md)
binary_printFL (Binary bstr, bmd) = addBString bstr


-----------------------------------------------------------------

-- | string with end character. Ex:
--
-- > StringC ','
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

stringC_genM :: Char -> PadsGen StringC
stringC_genM c = do
  i <- randNumBound 500
  replicateM i (randLetterExcluding c)

stringC_serialize :: Char -> StringC -> CList
stringC_serialize c s = (string_serialize s) `cAppend` (toCL [CharChunk c])

-----------------------------------------------------------------


-- | string of fixed length
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

stringFW_def :: Int -> StringFW
stringFW_def n = replicate n 'X'

stringFW_printFL :: Int -> PadsPrinter (StringFW, Base_md)
stringFW_printFL n (str, bmd)  = addString (take n str)

stringFW_genM :: Int -> PadsGen StringFW
stringFW_genM i = replicateM i randLetter

stringFW_serialize :: Int -> StringFW -> CList
stringFW_serialize _ = string_serialize


-----------------------------------------------------------------

-- | string of variable length
type StringVW = String
type StringVW_md = Base_md

stringVW_parseM :: Int -> PadsParser (StringVW, Base_md)
stringVW_parseM 0 = returnClean ""
stringVW_parseM n =
  handleEOF (stringVW_def n) "StringVW" $
  handleEOR (stringVW_def n) "StringVW" $ do
    str <- takeP n
    returnClean str

stringVW_def :: Int -> StringVW
stringVW_def n = replicate n 'X'

stringVW_printFL :: Int -> PadsPrinter (StringVW, Base_md)
stringVW_printFL n (str, bmd)  = addString (take n str)

stringVW_genM :: Int -> PadsGen StringVW
stringVW_genM x = do
  i <- randNumBound x
  replicateM i randLetter

stringVW_serialize :: Int -> StringVW -> CList
stringVW_serialize i s = string_serialize s

---- string of variable length (end if EOR)
--type StringVW = String
--type StringVW_md = Base_md
--
--stringVW_parseM :: (Bool,Int) -> PadsParser (StringVW, Base_md)
--stringVW_parseM (endIfEOR,0) = returnClean ""
--stringVW_parseM (endIfEOR,n) = do
--  let (doEOF, doEOR) = if endIfEOR then (checkEOF, checkEOR) else (handleEOF, handleEOR)
--  doEOF "" "StringVW" $ doEOR "" "StringVW" $ do
--    c1 <- takeHeadP
--    (rest, rest_md) <- stringVW_parseM (endIfEOR,pred n)
--    return (c1:rest, rest_md)
--
--stringVW_def (endIfEOR,n) = replicate n 'X'
--
--stringVW_printFL :: (Bool,Int) -> PadsPrinter (StringVW, Base_md)
--stringVW_printFL (endIfEOR,n) (str, bmd)  = addString (take n str)

-----------------------------------------------------------------

-----------------------------------------------------------------

-- | string with matching expression. For example:
--
-- > [pads| type StrME = StringME 'a+' |]
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

stringME_genM :: RE -> PadsGen StringME
stringME_genM _
  = error $ "stringME_genM unimplemented: consider using the workaround \"type "
         ++ "XYZ = obtain StringME (RE) from StringME (RE) using <| "
         ++ "(const id, id) |> generator (gen)\" in your description to "
         ++ "provide your own regex-specific generator."

stringME_serialize :: RE -> StringME -> CList
stringME_serialize _ s = string_serialize s

-----------------------------------------------------------------

-- | string matching given native regex. PADS uses posix regex (from the
--   regex-posix package). For example:
--
-- > [pads| StringSE <| RE "b|c" |>|]
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

stringSE_genM :: RE -> PadsGen StringSE
stringSE_genM r
  = error $ "stringSE_genM unimplemented: consider using the workaround \"type "
         ++ "XYZ = obtain StringSE (" ++ show r ++ ") from StringSE ("
         ++ show r ++ ") using <| (const id, id) |> generator (gen)\" in your "
         ++ "description to provide your own regex-specific generator."

stringSE_serialize :: RE -> StringSE -> CList
stringSE_serialize _ s = string_serialize s


-----------------------------------------------------------------

-- | string with a predicate. For example:
--
-- > [pads| type Digits = StringP Char.isDigit |]
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

stringP_genM :: (Char -> Bool) -> PadsGen StringP
stringP_genM _
  = error $ "stringP_genM unimplemented: consider using the workaround \"type "
         ++ "XYZ = obtain StringP (pred) from StringP (pred) using <| "
         ++ "(const id, id) |> generator (gen)\" in your description to "
         ++ "provide your own predicate-specific generator."


stringP_serialize :: (Char -> Bool) -> StringP -> CList
stringP_serialize _ s = string_serialize s


-----------------------------------------------------------------

-- | string predicate with escape condition
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

stringPESC_def :: (Bool, (Char, [Char])) -> String
stringPESC_def arg@(endIfEOR, (escape, stops)) = ""

stringPESC_printFL :: (Bool, (Char, [Char])) -> PadsPrinter (StringPESC, Base_md)
stringPESC_printFL (_, (escape, stops)) (str, bmd) =
  let replace c = if c `elem` stops then escape : [c] else [c]
      newStr =  concat (map replace str)
  in addString newStr

stringPESC_genM :: (Bool, (Char, [Char])) -> PadsGen StringPESC
stringPESC_genM _ = error "stringPESC_genM: unimplemented"

stringPESC_serialize :: (Bool, (Char, [Char])) -> StringPESC -> CList
stringPESC_serialize _ s = string_serialize s

-----------------------------------------------------------------
-- Non-byte-aligned (NB) types


type CharNB = Char
type CharNB_md = Base_md

charNB_parseM :: PadsParser (CharNB, Base_md)
charNB_parseM =
    handleEOF def "CharNB" $
    handleEOR def "CharNB" $ do
        c <- takeBitsP 8
        returnClean (S.word8ToChr (fromIntegral c :: Word8))

charNB_def :: Char
charNB_def = char_def

charNB_printFL :: PadsPrinter (CharNB, md)
charNB_printFL (c, bmd) = addString [c]

charNB_genM :: PadsGen CharNB
charNB_genM = char_genM

charNB_serialize :: CharNB -> CList
charNB_serialize = char_serialize

-----------------------------------------------------------------


type StringNB = String
type StringNB_md = Base_md

stringNB_parseM :: PadsParser (String, Base_md)
stringNB_parseM = do
    str <- drainSourceNBP
    returnClean str

stringNB_def = string_def

stringNB_printFL :: PadsPrinter (String, Base_md)
stringNB_printFL = string_printFL

stringNB_genM :: PadsGen StringNB
stringNB_genM = string_genM

-----------------------------------------------------------------

type StringCNB = String
type StringCNB_md = Base_md

stringCNB_parseM :: Char -> PadsParser (StringCNB, Base_md)
stringCNB_parseM c =
    handleEOF (stringCNB_def c) "StringCNB" $
    handleEOR (stringCNB_def c) "StringCNB" $ do
        str <- satisfyNBP (\c' -> c /= c')
        returnClean str

stringCNB_def :: Char -> StringCNB
stringCNB_def = stringC_def

stringCNB_printFL :: Char -> PadsPrinter (StringCNB, Base_md)
stringCNB_printFL = stringC_printFL

stringCNB_genM :: Char -> PadsGen StringCNB
stringCNB_genM = stringC_genM

stringCNB_serialize :: Char -> StringC -> CList
stringCNB_serialize = stringC_serialize


-----------------------------------------------------------------

type StringFWNB = String
type StringFWNB_md = Base_md

stringFWNB_parseM :: Int -> PadsParser (StringFW, Base_md)
stringFWNB_parseM 0 = returnClean ""
stringFWNB_parseM n =
    handleEOF (stringFWNB_def n) "StringFWNB" $
    handleEOR (stringFWNB_def n) "StringFWNB" $ do
        str <- takeBytesNBP n
        let str' = map S.word8ToChr (B.unpack str)
        if (length str') == n
            then returnClean str'
            else returnError (stringFWNB_def n) (E.Insufficient (length str') n)

stringFWNB_def :: Int -> StringFW
stringFWNB_def n = replicate n 'X'

stringFWNB_printFL :: Int -> PadsPrinter (StringFW, Base_md)
stringFWNB_printFL = stringFW_printFL

stringFWNB_genM :: Int -> PadsGen StringFWNB
stringFWNB_genM = stringFW_genM

stringFWNB_serialize :: Int -> StringFWNB -> CList
stringFWNB_serialize _ = string_serialize

-----------------------------------------------------------------

type BytesNB = S.RawStream
type BytesNB_md = Base_md

bytesNB_parseM :: Int -> PadsParser (BytesNB, BytesNB_md)
bytesNB_parseM n =
    handleEOF (def1 n) "BytesNB" $
    handleEOR (def1 n) "BytesNB" $ do
        bytes <- takeBytesNBP n
        if B.length bytes == n
            then returnClean bytes
            else returnError (def1 n) (E.Insufficient (B.length bytes) n)

bytesNB_printFL :: Int -> PadsPrinter (BytesNB, BytesNB_md)
bytesNB_printFL = bytes_printFL

bytesNB_def :: Int -> BytesNB
bytesNB_def = bytes_def

bytesNB_genM :: Int -> PadsGen BytesNB
bytesNB_genM = bytes_genM

bytesNB_serialize :: Int -> BytesNB -> CList
bytesNB_serialize = bytes_serialize

-----------------------------------------------------------------


-- | Chunks represent an abstraction of literal data, and allow for easy
-- consumption and concatenation into one ByteString of data, which can be
-- written to disk. Each BinaryChunk represents the value val .&. (2^bits - 1)
data Chunk = CharChunk   Char
           | BinaryChunk { val :: Integer, bits :: Int }
    deriving (Eq, Show, Lift)

-- | fromChunks provides a translation from Chunks to a list of bytes. It
-- accomplishes this in time linear to the length of the list of Chunks. It
-- converts each chunk into "bits" (a list of 1's and 0's), then splits that
-- into "bytes" (lists of length 8 each) to simplify combination in non-byte-
-- aligned cases.
fromChunks :: [Chunk] -> B.ByteString
fromChunks cs = let
  bits = concat $ chunksToBits cs
  toPad = case (8 - ((length bits) `mod` 8)) of 8 -> 0; x -> x
  padding = replicate toPad 0
  bytes = asBytes $ bits ++ padding
  in if   (length (bits ++ padding) `mod` 8) /= 0
     then error "fromChunks: bug in binary conversion"
     else B.pack $ map fromBytes bytes
  where
    -- | Dispatches to toPaddedBits depending on Chunk type
    chunksToBits :: [Chunk] -> [[Word8]]
    chunksToBits [] = []
    chunksToBits ((CharChunk c):cs) =
      (toPaddedBits (fromEnum c) 8) : chunksToBits cs
    chunksToBits ((BinaryChunk v b):cs) =
      (toPaddedBits v            b) : chunksToBits cs

    -- | Ensure every value includes the number of bits it's meant to, since
    -- toBits won't include 0s when necessary
    toPaddedBits :: Integral a => a -> Int -> [Word8]
    toPaddedBits x padTo = let
      x' = toBits x []
      padding = replicate (padTo - length x') 0
      in if   (padTo - length x') < 0
         then drop (abs $ padTo - length x') x'
         else padding ++ x'

    -- | Straightforward decimal-to-bits conversion
    toBits :: Integral a => a -> [Word8] -> [Word8]
    toBits 0 [] = [0]
    toBits 0 bs = bs
    toBits x bs = toBits (x `div` 2) (fromIntegral x `mod` 2 : bs)

    -- | Split list of 1s and 0s into lists of length 8 each
    asBytes :: [Word8] -> [[Word8]]
    asBytes [] = []
    asBytes xs = (take 8 xs) : (asBytes $ drop 8 xs)

    -- | Convert a list of 8 bits into a decimal byte
    fromBytes :: [Word8] -> Word8
    fromBytes bs = let
      withPowers = zip bs (reverse [0..7])
      in foldr1 (+) (map (\(b,p) -> b * 2^p) withPowers)

type CList = [Chunk] -> [Chunk]

-- For debugging mostly
instance Show CList where
  show cl = show $ fromCL cl

cAppend :: CList -> CList -> CList
cs1 `cAppend` cs2 = cs1 . cs2

cConcat :: [CList] -> CList
cConcat cl = foldr cAppend id cl

toCL :: [Chunk] -> CList
toCL cs = (cs ++)

fromCL :: CList -> [Chunk]
fromCL cl = cl []

class ExpSerialize a where
  exp_serialize :: a -> CList

instance ExpSerialize Char where
  exp_serialize = char_serialize

instance ExpSerialize [Char] where
  exp_serialize = string_serialize

instance ExpSerialize RE where
  exp_serialize _ = string_serialize "RegEx Literal"

-- This instance doesn't overlap with the above three, though GHC's constraint-
-- matching logic obscures that fact. Since a. no other instance is "strictly
-- more specific" and b. this instance is the only incoherent one, the
-- INCOHERENT pragma can be applied safely here without fear of undefined
-- behavior (likely failure) by GHC choosing a random/incorrect instance. See
-- the GHC user's guide at section 10.8.3.6 for more details.
instance {-# INCOHERENT #-} (Num a, Show a) => ExpSerialize a where
  exp_serialize x = toCL $ map CharChunk $ show x

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

-- | End of File
type EOF = ()
type EOF_md = Base_md

eof_parseM :: PadsParser (EOF, Base_md)
eof_parseM = do
  isEof <- isEOFP
  if isEof then returnClean ()
           else returnError () (E.ExtraBeforeLiteral "Eof")

-- | End of Record
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

eOR_def :: EOR
eOR_def = ()

eof_printFL :: (EOF,Base_md) -> FList
eof_printFL = const eofLit_printFL

eOR_genM :: PadsGen EOR
eOR_genM = return eOR_def

eOR_serialize :: CList
eOR_serialize = toCL [CharChunk '\n']

eOF_printFL = eof_printFL

eOF_def :: EOF
eOF_def = ()

eOF_genM :: PadsGen EOF
eOF_genM = return eOF_def

eOF_serialize :: CList
eOF_serialize = toCL []

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

void_def :: Void
void_def = Void ()

type instance Meta Void = Base_md
instance Pads1 () Void Base_md where
  parsePP1 () = void_parseM
  printFL1 () = void_printFL
  def1 () = void_def

void_printFL :: PadsPrinter (Void,Base_md)
void_printFL v = nil

void_genM :: PadsGen Void
void_genM = return void_def

void_serialize :: CList
void_serialize = toCL []



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

bytes_def :: Int -> Bytes
bytes_def i = B.pack $ replicate i (0::Word8)

bytes_genM :: Int -> PadsGen Bytes
bytes_genM i = do
  w8s <- replicateM i $ randNumBound (255 :: Word8)
  return $ B.pack w8s

bytes_serialize :: Int -> Bytes -> CList
bytes_serialize _ bs = toCL $ map (CharChunk . S.word8ToChr) $ B.unpack bs

type instance PadsArg Bytes = Int
type instance Meta Bytes = Bytes_md
instance Pads1 Int Bytes Bytes_md where
  parsePP1 = bytes_parseM
  printFL1 = bytes_printFL
  def1 i = bytes_def i


{- Helper functions -}
mkStr c = "'" ++ [c] ++ "'"

-- | Some PADS types, PConstrain for instance, are designed to have access to
-- parsed metadata, stored as the variable md. In parsing, metadata is created
-- and supplied to the constraint at the correct time in the generated parsing
-- functions.
-- However, during generation of generation functions, no metadata exists.
-- Providing this variable assignment prevents compile time errors of functions
-- with predicates that refer to md, and is safe wrt parsing predicates
-- because the md variables in their generated code are bound in lambdas.
md = Base_md 0 Nothing
