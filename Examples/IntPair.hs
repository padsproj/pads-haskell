{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}
module Examples.IntPair where
import Language.Pads.Padsc
import Data.Data

test = parseStringInput trip_parseM "354;;23|53|43(652%%35)4;hefd"


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

|]

[pads| 

type Triple = (Int, (obtain Char from Int using <|(bi,ib)|>, Int)) 


data May a = Jus a
           | Noth ()
|]





bi::a->(Int,Int_md)->(Char,Char_md)
bi x = undefined
ib y = undefined




