{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}
module Main where
import Language.Pads.Padsc hiding (take, rest, head)
import Language.Pads.GenPretty
import System.IO.Unsafe (unsafePerformIO)


[pads| type Strs = [Line StringLn] terminator EOF |]

{- should load with no errors -}
(rep,md) = unsafePerformIO $ parseFileWithD windows strs_parseM "Examples/data/fig-small.fig"

	
