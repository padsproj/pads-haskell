{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}
module DOS where
import Language.Pads.Padsc hiding (take, rest, head)
import Language.Pads.GenPretty
import System.IO.Unsafe (unsafePerformIO)


[pads| type Strs = [Line StringLn] terminator EOF |]

[pads| type StrsWindows = partition Strs using windows |]

{- should load with no errors -}
(rep,md) = unsafePerformIO $ parseFileWithD windows strs_parseM "data/fig-small.fig"

(rep1,md1) = unsafePerformIO $ parseFileWith strsWindows_parseM "data/fig-small.fig"
