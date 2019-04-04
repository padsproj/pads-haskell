{-# LANGUAGE FlexibleContexts, TypeFamilies, TypeSynonymInstances, TemplateHaskell, QuasiQuotes,
             MultiParamTypeClasses, FlexibleInstances, UndecidableInstances,
             DeriveDataTypeable, ScopedTypeVariables #-}
module ExtensibleEx where
import Language.Pads.Padsc
import Language.Haskell.TH

import Extensible

[myQuoter| type Foo x y z = (Int, '|', Int) |]

main :: IO ()
main = do
  if (karl_Foo == 3) -- because Foo has 3 type parameters
    then print "Success (:"
    else print "Failure ):"
  print $ (fst . fst . karl_Foo_parseS 0 0 0) "a|b"
  print $ (fst . fst . foo_parseS 7 8 9)      "3|4"

