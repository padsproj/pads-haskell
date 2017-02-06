{-# LANGUAGE DeriveDataTypeable #-}

{-
** *********************************************************************
*                                                                      *
*         (c)  Kathleen Fisher <kathleen.fisher@gmail.com>             *
*              John Launchbury <john.launchbury@gmail.com>             *
*                                                                      *
************************************************************************
-}

module Language.Pads.RegExp where
import Data.Data

-- | Regular expression support. PADS uses the regex-posix package.
data RE = RE String -- ^ A regular expression printed as its match
        | REd String String -- ^ A regular expression printed as the second arg
  deriving (Eq, Data, Typeable, Show)

