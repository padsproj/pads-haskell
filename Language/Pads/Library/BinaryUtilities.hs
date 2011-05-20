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


module Language.Pads.Library.BinaryUtilities where

import Data.Int
import Data.Word
import Data.ByteString as B
import Data.Bits
import System.ByteOrder

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

