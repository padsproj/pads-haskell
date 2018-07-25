{-# LANGUAGE TypeFamilies
           , ScopedTypeVariables
           , DeriveDataTypeable
           , MultiParamTypeClasses
           , TypeSynonymInstances
           , TemplateHaskell
           , QuasiQuotes
           , FlexibleInstances
           , FlexibleContexts
           , RecordWildCards
           , NamedFieldPuns
           , OverloadedStrings #-}

module Addrs where

import Language.Pads.Padsc
import Language.Pads.Testing
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString as B
import qualified Language.Pads.Library.LittleEndian as LE
import qualified Language.Pads.Library.BigEndian as BE

test = [pads|
    newtype Addresses = Addresses ([Address])

    data Address = Address {num :: Int, ' ', street :: StringC '\n'}
|]

testbits = [pads|
    data SomeBits = SomeBits {x :: Bits8 9, y :: Bits8 4}
|]
