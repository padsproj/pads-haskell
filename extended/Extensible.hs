{-# LANGUAGE FlexibleContexts, TypeFamilies, TypeSynonymInstances, TemplateHaskell, QuasiQuotes,
             MultiParamTypeClasses, FlexibleInstances, UndecidableInstances,
             DeriveDataTypeable, ScopedTypeVariables #-}
module Extensible where

import Language.Pads.Padsc
import Language.Pads.Syntax as PS
import Language.Pads.MetaData
import Language.Pads.Generic
import Language.Pads.PadsParser
import Language.Pads.CoreBaseTypes
import Language.Pads.TH
import qualified Language.Pads.Errors as E
import qualified Language.Pads.Source as S
import Language.Pads.PadsPrinter
import Language.Pads.Generation

import Language.Haskell.TH
-- import Language.Haskell.TH.Syntax

import Data.Data
import Data.Char
import qualified Data.Map as M
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Control.Monad
import Language.Haskell.TH.Syntax
import qualified System.Random.MWC as MWC
import qualified Debug.Trace as D

myCompiler :: [PadsDecl] -> Q [Dec]
myCompiler (x:xs) = compileSingle x
myCompiler [] = pure []

fI = fromIntegral

compileSingle :: PadsDecl -> Q [Dec]
compileSingle (PadsDeclType name typeParams pat inner generator) = [d| $(varP $ mkName $ "karl_" ++ name) = $(litE $ integerL $ fI $ length typeParams) |]
compileSingle (PadsDeclData name typeParams pat inner derivers)  = [d| $(varP $ mkName $ "karl_" ++ name) = $(litE $ integerL $ fI $ length typeParams) |]
compileSingle (PadsDeclNew  name typeParams pat inner derivers)  = [d| $(varP $ mkName $ "karl_" ++ name) = $(litE $ integerL $ fI $ length typeParams) |]
compileSingle (PadsDeclObtain name typeParams inner invers generator) = [d| $(varP $ mkName $ "karl_" ++ name) = $(litE $ integerL $ fI $ length typeParams) |]

myQuoter = padsE myCompiler

