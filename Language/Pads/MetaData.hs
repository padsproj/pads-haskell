{-# LANGUAGE Rank2Types, TypeFamilies, KindSignatures, NamedFieldPuns, RecordWildCards, FlexibleInstances, DeriveDataTypeable #-}
{-# LANGUAGE ConstraintKinds, MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables, FlexibleContexts #-}
{-
** *********************************************************************
*                                                                      *
*         (c)  Kathleen Fisher <kathleen.fisher@gmail.com>             *
*              John Launchbury <john.launchbury@gmail.com>             *
*                                                                      *
************************************************************************
-}


module Language.Pads.MetaData where

import qualified Language.Pads.Errors as E
import qualified Language.Pads.Source as S
import Text.PrettyPrint.Mainland as PP
import Text.PrettyPrint.Mainland.Class

import System.Posix.Types

import Data.Generics
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.List

{- Base type library support -}
data Base_md = Base_md { numErrors :: Int
                       , errInfo   :: Maybe E.ErrInfo
                        -- Need to add location information, etc.
                       }
   deriving (Typeable, Data, Eq, Ord, Show)


{- Meta data type class -}
class Data md => PadsMD md where
  get_md_header :: md -> Base_md
  replace_md_header :: md -> Base_md -> md

instance PadsMD Base_md where
  get_md_header b = b
  replace_md_header old new = new

instance Data b => PadsMD (Base_md,b) where
  get_md_header (h,b) = h
  replace_md_header (h1,b) h2 = (h2,b)

cleanBasePD = Base_md {numErrors = 0, errInfo = Nothing }
errorBasePD path = Base_md {numErrors = 1, errInfo = Just (E.ErrInfo (E.FileError path path) Nothing) }


mergeBaseMDs :: [Base_md] -> Base_md
mergeBaseMDs mds = foldl addInfo cleanBasePD mds
  where
    addInfo (Base_md {numErrors=num1,errInfo=i1}) 
            (Base_md {numErrors=num2,errInfo=i2})
      = Base_md {numErrors=num1 + num2, errInfo= E.maybeMergeErrInfo i1 i2 }


mkErrBasePDfromLoc msg loc 
  = Base_md {numErrors = 1, 
      errInfo = Just (E.ErrInfo{msg=msg,position= Just (S.locToPos loc)}) }  

mkErrBasePD msg pos
  = Base_md {numErrors = 1, 
      errInfo = Just (E.ErrInfo{msg=msg,position= pos}) }


instance Pretty Base_md where
  ppr = pprBaseMD

pprBaseMD Base_md {numErrors=num, errInfo = info} 
  = text "Errors:" <+> ppr num <+> 
    case info of
      Nothing -> PP.empty
      Just e -> ppr e

type family Meta (rep :: *) :: *

myempty :: forall a. Data a => a
myempty = general 
      `extB` char 
      `extB` int
      `extB` integer
      `extB` float 
      `extB` double 
      `extB` coff
      `extB` epochTime
      `extB` fileMode
      `ext2B` map
      `ext1B` set
      `ext1B` list where
  -- Generic case
  general :: Data a => a
  general = fromConstrB myempty (indexConstr (dataTypeOf general) 1)
  
  -- Base cases
  char    = '\NUL'
  int     = 0      :: Int
  integer = 0      :: Integer
  float   = 0.0    :: Float
  double  = 0.0    :: Double
  coff    = 0      :: COff
  epochTime = 0    :: EpochTime
  fileMode = 0     :: FileMode
  list :: Data b => [b]
  list    = []
  map :: (Data k,Data v) => Map k v
  map = Map.empty
  set :: Data k => Set k
  set = Set.empty
