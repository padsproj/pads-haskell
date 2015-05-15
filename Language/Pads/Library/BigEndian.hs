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


module Language.Pads.Library.BigEndian where

import Language.Pads.Padsc
import Language.Pads.Library.BinaryUtilities

import qualified Data.Int
import qualified Data.Word
import Data.ByteString as B

-- SIGNED INTEGERS --
-- type Int8 : 8-bit, signed integers
type Int8 = Data.Int.Int8
[pads| obtain Int8 from Bytes 1 using <|(bToi8sbh,i8Tobsbh)|> |]
bToi8sbh :: Pos -> (Bytes, Base_md) -> (Data.Int.Int8, Base_md)
bToi8sbh p (bytes,md) = (fromIntegral (bytes `B.index` 0), md)
i8Tobsbh (i,md) = (B.singleton (fromIntegral i), md)

type Int16 = Data.Int.Int16
--type Int16 : signed byte high, 16-bit, signed integers
[pads|  obtain Int16 from Bytes 2 using <| (bToi16sbh,i16sbhTob) |> |]
bToi16sbh p (bs,md) = (bytesToInt16 SBH bs, md)
i16sbhTob (i,md) = (int16ToBytes SBH i, md)

type Int32 = Data.Int.Int32
--type Int32 : signed byte high, 32-bit, signed integers
[pads| obtain Int32 from Bytes 4 using <| (bToi32sbh,i32sbhTob) |> |]
bToi32sbh p (bs,md) = (bytesToInt32 SBH bs, md)
i32sbhTob (i,md) = (int32ToBytes SBH i, md)


-- UNSIGNED INTEGERS (AKA WORDS) --
--type Word8 : signed byte high, 8-bit, unsigned integers
type Word8 = Data.Word.Word8
[pads| obtain Word8 from Bytes 1 using <|(bTow8,w8Tob)|> |]
bTow8 p (bytes,md) = (bytes `B.index` 0, md)
w8Tob (i,md) = (B.singleton i, md)

type Word16 = Data.Word.Word16
--type Word16 : signed byte high, 16-bit, unsigned integers
[pads| obtain Word16 from Bytes 2 using <| (bTow16sbh,w16sbhTob) |> |]
bTow16sbh p (bs,md) = (bytesToWord16 SBH bs, md)
w16sbhTob (i,md) = (word16ToBytes SBH i, md)

type Word32 = Data.Word.Word32
--type Word32 : signed byte high, 32-bit, unsigned integers
[pads| obtain Word32 from Bytes 4 using <| (bTow32sbh,w32sbhTob) |> |]
bTow32sbh p (bs,md) = (bytesToWord32 SBH bs, md)
w32sbhTob (i,md) = (word32ToBytes SBH i, md)

