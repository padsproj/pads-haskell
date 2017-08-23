{-# LANGUAGE FlexibleContexts, TypeFamilies, TemplateHaskell, QuasiQuotes, DeriveDataTypeable, ScopedTypeVariables, MultiParamTypeClasses,
    FlexibleInstances, TypeSynonymInstances, UndecidableInstances #-}


{-
** *********************************************************************
*                                                                      *
*         (c)  Kathleen Fisher <kathleen.fisher@gmail.com>             *
*              John Launchbury <john.launchbury@gmail.com>             *
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
import Language.Pads.PadsPrinter
import Data.Time
import System.Locale as Locale
import Text.PrettyPrint.Mainland (text)
import Text.PrettyPrint.Mainland.Class

import qualified Data.Char as C
import qualified Data.List as L
import Data.Data
import qualified Data.ByteString as B  


[pads|
-- string that stops in a newline
type StringEOR = [Char] terminator EOR
type Line a   = (a, EOR)
type StringLn = [Char] terminator (Try EOR)
type StringLnP (p :: String -> Bool) = constrain s :: StringLn where <| p s |> 
type StringESCLn (p :: (Char, [Char])) = StringPESC <|(True, p)|> 
type StringESC   (p :: (Char, [Char])) = StringPESC <|(False, p)|> 

data PMaybe a = PJust a
              | PNothing Void
obtain Maybe a from PMaybe a using <|(pm2m,m2pm)|>

|]

pm2m :: Pos -> (PMaybe a, PMaybe_md a_md) -> (Maybe a, Maybe_md a_md)
pm2m p (PJust x, md) = (Just x, md)
pm2m p (PNothing,md) = (Nothing,md)

m2pm :: (Maybe a, Maybe_md a_md) -> (PMaybe a, PMaybe_md a_md)
m2pm (Just x, md) = (PJust x, md)
m2pm (Nothing,md) = (PNothing,md)


[pads|
type Lit   (x::String) = (Void, x)
type LitRE (x::RE)     = (Void, x)
|]

[pads| obtain Bool from Bytes 1 using <|(bTobl,blTob)|> |]
bTobl :: Pos -> (Bytes,Bytes_md) -> (Bool,Bool_md)
bTobl p (bytes,md) = (fromIntegral (bytes `B.index` 0)==(1::Int), md)
blTob :: (Bool,Bool_md) -> (Bytes,Bytes_md)
blTob (b,md) = (B.singleton (if b then 1 else 0), md)


[pads| type DateFSE (fmt :: String, se :: RE) = obtain UTCTime from StringSE se using <| (strToUTC fmt, utcToStr fmt) |> 
       type DateFC (fmt::String, c::Char) = DateFSE <|(fmt, RE ("[" ++ [c] ++  "]")) |> |]  

type UTCTime_md = Base_md
instance Pretty UTCTime where
  ppr utc = text (show utc)


strToUTC :: String -> Pos -> (StringSE, Base_md) -> (UTCTime, Base_md)
strToUTC fmt pos (input, input_bmd) = 
  case parseTimeM True Data.Time.defaultTimeLocale fmt input of 
       Nothing -> (gdef, mergeBaseMDs [errPD, input_bmd])
       Just t  -> (t, input_bmd)
  where
    errPD = mkErrBasePD (TransformToDstFail "DateFSE" input " (conversion failed)") (Just pos)

uTCTime_def = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

utcToStr :: String -> (UTCTime, Base_md) -> (StringSE, Base_md) 
utcToStr fmt (utcTime, bmd) = (formatTime Data.Time.defaultTimeLocale fmt utcTime, bmd)


[pads| type TimeZoneSE (se :: RE) = obtain TimeZone from StringSE se using <| (strToTz, tzToStr) |> 
       type TimeZoneC (c::Char) = TimeZoneSE <|RE ("[" ++ [c] ++  "]") |> |]  

type TimeZone_md = Base_md
instance Pretty TimeZone where
  ppr tz = text (show tz)

strToTz :: Pos -> (StringSE, Base_md) -> (TimeZone, Base_md)
strToTz pos (input, input_bmd) = 
  case parseTimeM True Data.Time.defaultTimeLocale "%z" input of 
       Nothing -> (gdef,  mergeBaseMDs [mkErrBasePD (TransformToDstFail "TimeZoneSE" input " (conversion failed)") (Just pos), input_bmd])
       Just t  -> (t, input_bmd)

tzToStr ::  (TimeZone, Base_md) -> (StringSE, Base_md) 
tzToStr (tz, bmd) = (h ++ ":" ++ m, bmd)
           where (h,m) = splitAt 3 (show tz)

timeZone_def = utc

[pads| type Phex32FW (size :: Int) = obtain Int from StringFW size using <| (hexStr2Int,int2HexStr size) |> |]  

hexStr2Int :: Pos -> (StringFW, Base_md) -> (Int, Base_md)
hexStr2Int src_pos (s,md) = if good then (intList2Int ints 0, md)
                                      else (0, mkErrBasePD  (TransformToDstFail "StrHex" s " (non-hex digit)") (Just src_pos))
  where
    hc2int c = if C.isHexDigit c then (C.digitToInt c,True) else (0,False)
    (ints,bools) = unzip (map hc2int s)
    good = (L.and bools) && (length ints > 0)
    intList2Int digits a = case digits of
        []     -> a
        (d:ds) -> intList2Int ds ((16 * a) + d)

int2HexStr :: Int -> (Int, Base_md) -> (StringFW, Base_md)
int2HexStr size (x,md)
  | length result == size && wasPos = (result, md)       
  | not wasPos = (Prelude.take size result,    
                  mkErrBasePD (TransformToSrcFail "StrHex" (show x) (" (Expected positive number)")) Nothing)
  | otherwise  = (Prelude.take size result,
                  mkErrBasePD (TransformToSrcFail "StrHex" (show x) (" (too big to fit in "++ (show size) ++" characters)")) Nothing)
  where
   cvt rest a = if rest < 16 then {- reverse $ -} (C.intToDigit rest) : a
                else cvt (rest `div` 16) (C.intToDigit (rest `mod` 16) : a)
   (wasPos,x') = if x < 0 then (False, -x) else (True, x)
   temp = cvt x' []
   padding = size - (length temp)
   stutter c n = if n <= 0 then [] else c : (stutter c (n-1))
   result = (stutter '0' padding) ++ temp









