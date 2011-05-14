{-# LANGUAGE DeriveDataTypeable #-}

module Language.Pads.RegExp where
import Data.Data

{- Regular expression support -}
data RE = RE String | REd String String
  deriving (Eq, Data, Typeable, Show)

