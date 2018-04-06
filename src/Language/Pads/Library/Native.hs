{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, ScopedTypeVariables, MultiParamTypeClasses,
    FlexibleInstances, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_HADDOCK prune #-}
{-|
  Module      : Language.Pads.Library.Native
  Description : Support for parsing of native byte order data
  Copyright   : (c) 2011
                Kathleen Fisher <kathleen.fisher@gmail.com>
                John Launchbury <john.launchbury@gmail.com>
  License     : MIT
  Maintainer  : Karl Cronburg <karl@cs.tufts.edu>
  Stability   : experimental

-}
module Language.Pads.Library.Native where

import Language.Pads.Padsc
import Language.Pads.Library.BinaryUtilities

import qualified Data.Int
import qualified Data.Word
import Data.ByteString as B

-------------------------------------------------------------------------------
-- * Signed Integers
-- | type Int8 : 8-bit, signed integers
type Int8 = Data.Int.Int8
[pads| obtain Int8 from Bytes 1 using <|(bToi8,i8Tob)|> |]
bToi8 :: Span -> (Bytes, Base_md) -> (Data.Int.Int8, Base_md)
bToi8 p (bytes,md) = (fromIntegral (bytes `B.index` 0), md)
i8Tob :: (Int8,Int8_md) -> (Bytes,Bytes_md)
i8Tob (i,md) = (B.singleton (fromIntegral i), md)


-- | type Int16 : 16-bit, signed integers; bytes assembled in order
type Int16 = Data.Int.Int16
[pads| obtain Int16 from Bytes 2 using <| (bToi16,i16Tob) |> |]
bToi16 :: Span -> (Bytes,Bytes_md) -> (Int16,Int16_md)
bToi16 p (bs,md) = (bytesToInt16 Native bs, md)
i16Tob :: (Int16,Int16_md) -> (Bytes,Bytes_md)
i16Tob (i,md) = (int16ToBytes Native i, md)


-- | type Int32 : 32-bit, signed integers; bytes assembled in order
type Int32 = Data.Int.Int32
[pads| obtain Int32 from Bytes 4 using <|(bToi32,i32Tob)|> |]
bToi32 :: Span -> (Bytes,Bytes_md) -> (Int32,Int32_md)
bToi32 p (bytes,md) = (bytesToInt32 Native bytes, md)
i32Tob :: (Int32,Int32_md) -> (Bytes,Bytes_md)
i32Tob (i,md) = (int32ToBytes Native i, md)

-------------------------------------------------------------------------------
-- * Unsigned Integers (aka Words)
-- | type Word8 :  8-bit, unsigned integers, raw order
type Word8 = Data.Word.Word8
[pads| obtain Word8 from Bytes 1 using <|(bTow8,w8Tob)|> |]
bTow8 :: Span -> (Bytes,Bytes_md) -> (Word8,Word8_md)
bTow8 p (bytes,md) = (bytes `B.index` 0, md)
w8Tob :: (Word8,Word8_md) -> (Bytes,Bytes_md)
w8Tob (i,md) = (B.singleton i, md)

-- | type Word16 :  16-bit, unsigned integers, raw order
type Word16 = Data.Word.Word16
[pads| obtain Word16 from Bytes 2 using <|(bTow16,w16Tob)|> |]
bTow16 :: Span -> (Bytes,Bytes_md) -> (Word16,Word16_md)
bTow16 p (bytes,md) = (bytesToWord16 Native bytes, md)
w16Tob :: (Word16,Word16_md) -> (Bytes,Bytes_md)
w16Tob (i,md) = (word16ToBytes Native i, md)

-- | type Word32 :  32-bit, unsigned integers, raw order
type Word32 = Data.Word.Word32
[pads| obtain Word32 from Bytes 4 using <|(bTow32,w32Tob)|> |]
bTow32 :: Span -> (Bytes,Bytes_md) -> (Data.Word.Word32,Word32_md)
bTow32 p (bytes,md) = (bytesToWord32 Native bytes, md)
w32Tob :: (Data.Word.Word32,Word32_md) -> (Bytes,Bytes_md)
w32Tob (i,md) = (word32ToBytes Native i, md)


