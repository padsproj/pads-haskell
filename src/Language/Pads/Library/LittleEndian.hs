{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, ScopedTypeVariables, MultiParamTypeClasses,
    FlexibleInstances, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_HADDOCK prune #-}
{-|
  Module      : Language.Pads.Library.LittleEndian
  Description : Support for parsing of little endian data
  Copyright   : (c) 2011
                Kathleen Fisher <kathleen.fisher@gmail.com>
                John Launchbury <john.launchbury@gmail.com>
  License     : MIT
  Maintainer  : Karl Cronburg <karl@cs.tufts.edu>
  Stability   : experimental

-}
module Language.Pads.Library.LittleEndian where

import Language.Pads.Padsc
import Language.Pads.Library.BinaryUtilities

import qualified Data.Int
import qualified Data.Word
import Data.ByteString as B

-------------------------------------------------------------------------------
-- * Signed Integers
-- | type Int8 : signed byte low, 8-bit, signed integers
type Int8 = Data.Int.Int8
[pads| obtain Int8 from Bytes 1 using <|(bToi8,i8Tob)|> |]
bToi8 :: Span -> (Bytes, Base_md) -> (Data.Int.Int8, Base_md)
bToi8 p (bytes,md) = (fromIntegral (bytes `B.index` 0), md)
i8Tob (i,md) = (B.singleton (fromIntegral i), md)

int8_genM :: PadsGen Int8
int8_genM = randNum


-- | type Int16 : signed byte low, 16-bit, signed integers
type Int16 = Data.Int.Int16
[pads| obtain Int16 from Bytes 2 using <| (bToi16sbl,i16sblTob) |> |]
bToi16sbl p (bs,md) = (bytesToInt16 SBL bs, md)
i16sblTob (i,md) = (int16ToBytes SBL i, md)

int16_genM :: PadsGen Int16
int16_genM = randNum


-- | type Int32 : signed byte low, 32-bit, signed integers
type Int32 = Data.Int.Int32
[pads| obtain Int32 from Bytes 4 using <| (bToi32sbl,i32sblTob) |> |]
bToi32sbl p (bs,md) = (bytesToInt32 SBL bs, md)
i32sblTob (i,md) = (int32ToBytes SBL i, md)

int32_genM :: PadsGen Int32
int32_genM = randNum


-------------------------------------------------------------------------------
-- * Unsigned Integers (aka Words)
-- | type Word8 : signed byte low, 8-bit, unsigned integers
type Word8 = Data.Word.Word8
[pads| obtain Word8 from Bytes 1 using <|(bTow8,w8Tob)|> |]
bTow8 p (bytes,md) = (bytes `B.index` 0, md)
w8Tob (i,md) = (B.singleton i, md)

word8_genM :: PadsGen Word8
word8_genM = randNum


type Word16 = Data.Word.Word16
-- | type Word16 : signed byte low, 16-bit, unsigned integers
[pads| obtain Word16 from Bytes 2 using <| (bTow16sbl,w16sblTob) |> |]
bTow16sbl p (bs,md) = (bytesToWord16 SBL bs, md)
w16sblTob (i,md) = (word16ToBytes SBL i, md)

word16_genM :: PadsGen Word16
word16_genM = randNum


type Word32 = Data.Word.Word32
-- | type Word32 : signed byte low, 32-bit, unsigned integers
[pads| obtain Word32 from Bytes 4 using <| (bTow32sbl,w32sblTob) |> |]
bTow32sbl p (bs,md) = (bytesToWord32 SBL bs, md)
w32sblTob (i,md) = (word32ToBytes SBL i, md)

word32_genM :: PadsGen Word32
word32_genM = randNum
