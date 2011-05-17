{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, 
    MultiParamTypeClasses, FlexibleInstances, UndecidableInstances,
    DeriveDataTypeable, ScopedTypeVariables #-}
module Examples.IntPair where
import Language.Pads.Source
import Language.Pads.Errors 
import Language.Pads.Generic
import Language.Pads.MetaData
import Language.Pads.CoreBaseTypes
import Language.Pads.Quote
import Language.Pads.RegExp
import Language.Pads.LazyList

import Data.Data

--test = parseStringInput trip_parseM "354;;23|53|43(652%%35)4;hefd"


[pads| 

type IntPair a = (Int,":",a)

|]

{-
[pads| 

type Trip = (Int, ";;", Lst CharPair, Foo Int , EOR) 

newtype Lst a = Lst ([a | '|'] length 3)
  deriving Read

data CharPair = Char2 {first::Char,second::Char}

data Foo b = Bazz Int | Zipp "(" b '[01]+' !Int ')'
  deriving (Read, Eq)

data Switcher (x::Int) = case x of
                      0 -> Zero "$"
                    | 1 -> One (Lst Int)
                         deriving (Read, Eq)

type Triple = (Int, (obtain Char from Int using <|(bi,ib)|>, Int)) 

type MyVoid = ()


data May a = Jus a
           | Noth ()


data Exxy a = Exxy {exxy :: Int, aa :: a}



|]





bi::a->(Int,Int_md)->(Char,Char_md)
bi x = undefined
ib y = undefined

-}


