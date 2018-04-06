{-# LANGUAGE TypeFamilies, TypeSynonymInstances, TemplateHaskell, QuasiQuotes, 
             MultiParamTypeClasses, FlexibleInstances, UndecidableInstances,
             DeriveDataTypeable, ScopedTypeVariables #-}

module One where
import Language.Pads.Padsc

{-
[pads| type IntPair = (Int, '|', Int) |]
intPair_result = intPair_parseS "12|23"
intPair_expects =  ((12,23), 0,"")
-}

[pads|
  data Version = Version {"HTTP/", major :: Int, '.', minor :: Int}  -- add constriants on major and minor mode
  |]

{-
[pads| data  Record (bound::Int) = Record 
                {  i1 :: Int, 
              ',', i2 :: Int where <| i1 + i2 <= bound |>   } |]

-}

[pads| data  Record (bound::Int) = Record 
                {  i1 :: Int, 
              ',', i2 :: Int where <| i1 + i2 <= bound |>   } |]