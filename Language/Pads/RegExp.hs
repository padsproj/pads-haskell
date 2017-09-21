{-# LANGUAGE DeriveDataTypeable #-}
{-|
  Module      : Language.Pads.RegExp
  Description : Pads regular expressions
  Copyright   : (c) 2011
                Kathleen Fisher <kathleen.fisher@gmail.com>
                John Launchbury <john.launchbury@gmail.com>
  License     : MIT
  Maintainer  : Karl Cronburg <karl@cs.tufts.edu>
  Stability   : experimental

-}
module Language.Pads.RegExp where
import Data.Data

-- | Regular expression support. PADS uses the regex-posix package.
data RE = RE String -- ^ A regular expression printed as its match
        | REd String String -- ^ A regular expression printed as the second arg
  deriving (Eq, Data, Typeable, Show)

