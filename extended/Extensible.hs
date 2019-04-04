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

mkKarl name typeParams =
  [d| $(varP $ mkName $ "karl_" ++ name) = $(litE $ integerL $ fI $ length typeParams) |]

compileSingle :: PadsDecl -> Q [Dec]
compileSingle p@(PadsDeclType name typeParams pat inner generator)       =
  do k <- mkKarl name typeParams
     ds <- make_pads_declarations [transform p]
     pure $ k ++ ds
compileSingle p@(PadsDeclData name typeParams pat inner derivers)        = mkKarl name typeParams
compileSingle p@(PadsDeclNew  name typeParams pat inner derivers)        = mkKarl name typeParams
compileSingle p@(PadsDeclObtain name typeParams inner invers generator)  = mkKarl name typeParams

transform (PadsDeclType name typeParams pat inner generator)
  = PadsDeclType ("Karl_" ++ name) typeParams pat (transformInner inner) generator
transform _ = error "unimplemented"

transformInner (PTuple tys) = PTuple $ map transformInner tys
transformInner (PTycon qs)
  | qs == ["Int"] = PTycon ["Char"]
  | otherwise     = PTycon qs
transformInner rest = rest

myQuoter = padsE myCompiler

