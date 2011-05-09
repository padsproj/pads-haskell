{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, DeriveDataTypeable #-}
module Language.Pads.DeriveExample where

import Data.DeriveTH                 -- Library for deriving instances for existing types

import Data.IP 
import Data.Time.Clock
import Data.Time.Calendar

import Data.Data
import Language.Pads.Padsc

{- Example for simple types, defined locally -}
data Color = RGB Int Int Int
           | CMYK Int Int Int Int

data Bigger = B1 Color | B2 Int

(derive makeEq ''Color)
(derive makeEq ''Bigger)


{- Make IPv4 an instance of Typeable and Data -}
(derive makeTypeable ''IPv4)
(derive makeDataAbstract ''IPv4)

{- Example declarations to make Pip a Pads base type -}
newtype Pip = Pip IPv4
  deriving (Typeable, Data, Show, Eq)

instance Pads Pip Base_md where
  parsePP = pip_parseM

pip_parseM :: PadsParser (Pip, Base_md)
pip_parseM = undefined                          -- **** Needs to be defined ******

{- Make UTCTime an instance of Typeable and Data -}
--(derive makeDataAbstract ''DiffTime)
-- (derive makeData         ''UTCTime)
-- (derive makeData         ''Day)

{- Example declarations to make Ptime a Pads base type -}
instance Pretty UTCTime where
   ppr (UTCTime day sec) = text $ show (UTCTime day sec)

newtype Ptime = Ptime UTCTime
  deriving (Typeable, Data, Eq, Show)

instance Pads Ptime Base_md where
  parsePP = ptime_parseM

ptime_parseM :: PadsParser (Ptime, Base_md)
ptime_parseM = undefined                        -- **** Needs to be defined ******

