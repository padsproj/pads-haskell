{-# LANGUAGE FlexibleContexts, TypeFamilies, TypeSynonymInstances, TemplateHaskell, QuasiQuotes,
             MultiParamTypeClasses, FlexibleInstances, UndecidableInstances,
             DeriveDataTypeable, ScopedTypeVariables #-}
module ExtensibleEx where
import Language.Pads.Padsc
import Language.Haskell.TH

import Extensible

[myQuoter| type Foo x y z = (Int, '|', Int) |]

main :: IO ()
main =
  if (karl_Foo == 3) -- because Foo has 3 type parameters
    then print "Success (:"
    else print "Failure ):"

