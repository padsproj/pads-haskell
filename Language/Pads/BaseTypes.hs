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
import Data.Word
import Data.Bits
import System.ByteOrder


[pads|
type Line a   = (a, EOR)
type StringLn = [Char] terminator EOR
type StringLnP (p :: String -> Bool) = constrain s :: StringLn where <| p s |> 

data PMaybe a = PJust a
              | PNothing Void
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



-- type Int32 : 32-bit, signed integers; bytes assembled in order
type Int32_md = Base_md
[pads| obtain Int32 from Bytes 4 using <|(bToi32,i32Tob)|> |]

bToi32 p (bytes,md) = (bytesToInt32 Native bytes, md)
i32Tob (i,md) = (int32ToBytes Native i, md)

--type Int32sbh : signed byte high, 32-bit, signed integers
[pads| type Int32sbh = obtain Int32 from Bytes 4 using <| (bToi32sbh,i32sbhTob) |> |]

bToi32sbh p (bs,md) = (bytesToInt32 SBH bs, md)
i32sbhTob (i,md) = (int32ToBytes SBH i, md)


--type Int32sbh : signed byte high, 32-bit, signed integers
[pads| type Int32sbl = obtain Int32 from Bytes 4 using <| (bToi32sbl,i32sblTob) |> |]

bToi32sbl p (bs,md) = (bytesToInt32 SBH bs, md)
i32sblTob (i,md) = (int16ToBytes SBL i, md)


-- UNSIGNED INTEGERS --

type Word8_md = Base_md
[pads| obtain Word8 from Bytes 1 using <|(bTow8,w8Tob)|> |]
bTow8 p (bytes,md) = (bytes `B.index` 0, md)
w8Tob (i,md) = (B.singleton i, md)


type Word16_md = Base_md
[pads| obtain Word16 from Bytes 2 using <|(bTow16,w16Tob)|> |]
bTow16 p (bytes,md) = (bytesToWord16 Native bytes, md)
w16Tob (i,md) = (word16ToBytes Native i, md)


--type Word16sbh : signed byte high, 16-bit, unsigned integers
[pads| type Word16sbh = obtain Int16 from Bytes 2 using <| (bTow16sbh,w16sbhTob) |> |]
bTow16sbh p (bs,md) = (bytesToWord16 SBH bs, md)
w16sbhTob (i,md) = (word16ToBytes SBH i, md)


--type Word16sbl : signed byte low, 16-bit, unsigned integers
[pads| type Word16sbl = obtain Int16 from Bytes 2 using <| (bTow16sbl,w16sblTob) |> |]
bTow16sbl p (bs,md) = (bytesToWord16 SBL bs, md)
w16sblTob (i,md) = (word16ToBytes SBL i, md)


--type Word32 :  32-bit, unsigned integers, raw order
type Word32_md = Base_md
[pads| obtain Word32 from Bytes 4 using <|(bTow32,w32Tob)|> |]
bTow32 p (bytes,md) = (bytesToWord32 Native bytes, md)
w32Tob (i,md) = (word32ToBytes Native i, md)


--type Word32sbh : signed byte high, 32-bit, unsigned integers
[pads| type Word32sbh = obtain Int32 from Bytes 4 using <| (bTow32sbh,w32sbhTob) |> |]
bTow32sbh p (bs,md) = (bytesToWord32 SBH bs, md)
w32sbhTob (i,md) = (word32ToBytes SBH i, md)


--type Word32sbl : signed byte low, 32-bit, unsigned integers
[pads| type Word32sbl = obtain Int32 from Bytes 4 using <| (bTow32sbl,w32sblTob) |> |]
bTow32sbl p (bs,md) = (bytesToWord32 SBL bs, md)
w32sblTob (i,md) = (word32ToBytes SBL i, md)



----------------------------------

-- SIGNED-BYTE-HIGH FUNCTIONS --
-- The most significant digit is on the left (lowest address).
-- These functions manage the case where the wire representation
-- is signed-byte high, regardless of the endianness of the host
-- machine.
data Endian = SBH | SBL | Native

bytesToInt16 :: Endian -> B.ByteString -> Int16
bytesToInt16 endian = fromIntegral . (bytesToWord16 endian)

int16ToBytes :: Endian -> Int16 -> B.ByteString
int16ToBytes endian = (word16ToBytes endian) . fromIntegral


bytesToInt32 :: Endian -> B.ByteString -> Int32
bytesToInt32 endian = fromIntegral . (bytesToWord32 endian)

int32ToBytes :: Endian -> Int32 -> B.ByteString
int32ToBytes endian = (word32ToBytes endian) . fromIntegral


bytesToWord16 :: Endian -> B.ByteString -> Word16
bytesToWord16 endian b = 
  let  b0 :: Word16 = fromIntegral (b `B.index` 0)
       b1 :: Word16 = fromIntegral (b `B.index` 1)
  in
    case (endian, byteOrder) of
     (SBH, BigEndian)    ->   assembleWord16 (b1, b0)
     (SBH, LittleEndian) ->   assembleWord16 (b0, b1)
     (SBL, BigEndian)    ->   assembleWord16 (b0, b1)
     (SBL, LittleEndian) ->   assembleWord16 (b1, b0)
     (Native,  BigEndian)    ->   assembleWord16 (b0, b1)
     (Native, LittleEndian)  ->   assembleWord16 (b1, b0)


word16ToBytes :: Endian -> Word16 -> B.ByteString 
word16ToBytes endian word16 = 
  let w0 :: Word8 = fromIntegral (shiftR (word16 .&. 0xFF00)  8)
      w1 :: Word8 = fromIntegral         (word16 .&. 0x00FF)  
  in case (endian, byteOrder) of
     (SBH, BigEndian)    ->   B.pack [w1,w0]
     (SBH, LittleEndian) ->   B.pack [w0,w1]
     (SBL, BigEndian)    ->   B.pack [w0,w1]
     (SBL, LittleEndian) ->   B.pack [w1,w0]
     (Native, BigEndian) ->   B.pack [w0,w1]
     (Native, LittleEndian) ->   B.pack [w1,w0]
      

assembleWord16 :: (Word16, Word16) -> Word16
assembleWord16 (b0, b1) = shift b0 8 .|. b1



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

-------------------------------------------


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









