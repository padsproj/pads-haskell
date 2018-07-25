{-# LANGUAGE TypeSynonymInstances, GeneralizedNewtypeDeriving, QuasiQuotes, TemplateHaskell, DeriveDataTypeable,
             ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
             UndecidableInstances, NamedFieldPuns, RecordWildCards, StandaloneDeriving   #-}
{-# OPTIONS_HADDOCK prune #-}
{-|
  Module      : Language.Pads.Padsc
  Description : Pads compiler
  Copyright   : (c) 2011
                Kathleen Fisher <kathleen.fisher@gmail.com>
                John Launchbury <john.launchbury@gmail.com>
  License     : MIT
  Maintainer  : Karl Cronburg <karl@cs.tufts.edu>
  Stability   : experimental

  This module re-exports all the modules necessary to make use of the Pads
  quasiquoter and compilation infrastructure.

-}
module Language.Pads.Padsc (
     {- FIX: Languages.Pads.Source exports "take", which clashes with
        the function of the same name in Prelude -}
     module Language.Pads.Source,
     module Language.Pads.RegExp,
     module Language.Pads.Errors,
     module Language.Pads.PadsParser,
     module Language.Pads.MetaData,
     module Language.Pads.Generic,
     module Language.Pads.CoreBaseTypes,
     module Language.Pads.Quote,
     module Language.Pads.Syntax,
     module Language.Pads.BaseTypes,
     module Language.Pads.Pretty,
     module Language.Pads.PadsPrinter,
     module Language.Pads.Generation,
     module Data.Data,
     module Data.List,
     ppr, pretty
  )
  where

import Language.Pads.Source hiding (take, span, head, tail)
import Language.Pads.RegExp
import Language.Pads.Errors  hiding (msg)
import Language.Pads.PadsParser
import Language.Pads.MetaData
import Language.Pads.Generic
import Language.Pads.CoreBaseTypes
import Language.Pads.Quote
import Language.Pads.Syntax
import Language.Pads.BaseTypes
import Language.Pads.Pretty
import Language.Pads.PadsPrinter
import Language.Pads.Generation

import Data.Data
import Data.List
import Text.PrettyPrint.Mainland hiding (line,  dot)
import Text.PrettyPrint.Mainland.Class

{- Fix these should be reexported -}
-- parseAllS = Language.Pads.PadsParser.parseAllS
numErrors = Language.Pads.MetaData.numErrors
