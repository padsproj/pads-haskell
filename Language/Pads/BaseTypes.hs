{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, ScopedTypeVariables, MultiParamTypeClasses,
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
import Language.Pads.LazyList
import Data.Time
import System.Locale
import Text.PrettyPrint.Mainland

import qualified Data.Char as C
import qualified Data.List as L
import Data.Data
import qualified Data.ByteString as B  
import Data.Int


[pads|
type Line a   = (a, EOR)
type StringLn = [Char] terminator EOR
type StringLnP (p :: String -> Bool) = constrain s :: StringLn where <| p s |> 

data PMaybe a = PJust a
              | PNothing
obtain Maybe a from PMaybe a using <|(pm2m,m2pm)|>
|]
type Maybe_md a = PMaybe_md a

pm2m :: Pos -> (PMaybe a, md) -> (Maybe a, md)
pm2m p (PJust x, md) = (Just x, md)
pm2m p (PNothing,md) = (Nothing,md)

m2pm :: (Maybe a, Maybe_md a) -> (PMaybe a, PMaybe_md a)
m2pm (Just x, md) = (PJust x, md)
m2pm (Nothing,md) = (PNothing,md)


[pads|
type Lit   (x::String) = (Void, x)
type LitRE (x::RE)     = (Void, x)
|]


-- type Int8 : 8-bit, signed integers
type Int8_md = Base_md

[pads| obtain Int8 from Bytes 1 using <|(bToi8,i8Tob)|> |]

bToi8 :: pos -> (Bytes,Bytes_md) -> (Int8,Int8_md)
bToi8 p (bytes,md) = (fromIntegral (bytes `B.index` 0), md)
i8Tob (i,md) = (B.singleton (fromIntegral i), md)


-- type Int16 : 16-bit, signed integers; bytes assembled in order
type Int16_md = Base_md
[pads| obtain Int16 from Bytes 2 using <| (bToi16,i16Tob) |> |]

bToi16 :: pos -> (Bytes,Bytes_md) -> (Int16,Int16_md)
bToi16 p (bs,md) = (bytesToInt16 Native bs, md)
i16Tob (i,md) = (int16ToBytes Native i, md)

--type Int16sbh : signed byte high, 16-bit, signed integers
[pads| type Int16sbh = obtain Int16 from Bytes 2 using <| (bToi16sbh,i16sbhTob) |> |]

bToi16sbh :: pos -> (Bytes,Bytes_md) -> (Int16,Int16_md)
bToi16sbh p (bs,md) = (bytesToInt16 SBH bs, md)
i16sbhTob (i,md) = (int16ToBytes SBH i, md)

--type Int16sbl : signed byte low, 16-bit, signed integers
[pads| type Int16sbl = obtain Int16 from Bytes 2 using <| (bToi16sbl,i16sblTob) |> |]

bToi16sbl :: pos -> (Bytes,Bytes_md) -> (Int16,Int16_md)
bToi16sbl p (bs,md) = (bytesToInt16 SBH bs, md)
i16sblTob (i,md) = (int16ToBytes SBL i, md)




[pads| type DateFSE (fmt :: String, se :: RE) = obtain UTCTime from StringSE se using <| (strToUTC fmt, utcToStr fmt) |> 
       type DateFC (fmt::String, c::Char) = DateFSE <|(fmt, RE ("[" ++ [c] ++  "]")) |> |]  

type UTCTime_md = Base_md
instance Pretty UTCTime where
  ppr utc = text (show utc)


strToUTC :: String -> Pos -> (StringSE, Base_md) -> (UTCTime, Base_md)
strToUTC fmt pos (input, input_bmd) = 
  case parseTime defaultTimeLocale fmt input of 
       Nothing -> (gdef, mergeBaseMDs [errPD, input_bmd])
       Just t  -> (t, input_bmd)
  where
    errPD = mkErrBasePD (TransformToDstFail "DateFSE" input " (conversion failed)") (Just pos)

utcToStr :: String -> (UTCTime, Base_md) -> (StringSE, Base_md) 
utcToStr fmt (utcTime, bmd) = (formatTime defaultTimeLocale fmt utcTime, bmd)


[pads| type TimeZoneSE (se :: RE) = obtain TimeZone from StringSE se using <| (strToTz, tzToStr) |> 
       type TimeZoneC (c::Char) = TimeZoneSE <|RE ("[" ++ [c] ++  "]") |> |]  

type TimeZone_md = Base_md
instance Pretty TimeZone where
  ppr tz = text (show tz)

strToTz :: Pos -> (StringSE, Base_md) -> (TimeZone, Base_md)
strToTz pos (input, input_bmd) = 
  case parseTime defaultTimeLocale "%z" input of 
       Nothing -> (gdef,  mergeBaseMDs [mkErrBasePD (TransformToDstFail "TimeZoneSE" input " (conversion failed)") (Just pos), input_bmd])
       Just t  -> (t, input_bmd)

tzToStr ::  (TimeZone, Base_md) -> (StringSE, Base_md) 
tzToStr (tz, bmd) = (h ++ ":" ++ m, bmd)
           where (h,m) = splitAt 3 (show tz)


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
int2HexStr size (x,md) = if (length result == size) && wasPos  then (result, md)       
                              else if not wasPos then 
                                   (Prelude.take size result,    
                                    mkErrBasePD (TransformToSrcFail "StrHex" (show x) (" (Expected positive number)")) Nothing)
                              else (Prelude.take size result,
                                    mkErrBasePD (TransformToSrcFail "StrHex" (show x) (" (too big to fit in "++ (show size) ++" characters)")) Nothing)
  where
   cvt rest a = if rest < 16 then {- reverse $ -} (C.intToDigit rest) : a
                else cvt (rest `div` 16) (C.intToDigit (rest `mod` 16) : a)
   (wasPos,x') = if x < 0 then (False, -x) else (True, x)
   temp = cvt x' []
   padding = size - (length temp)
   stutter c n = if n <= 0 then [] else c : (stutter c (n-1))
   result = (stutter '0' padding) ++ temp









