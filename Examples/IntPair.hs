{-# LANGUAGE FlexibleContexts, TypeFamilies, TypeSynonymInstances, TemplateHaskell, QuasiQuotes, 
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
import qualified Language.Pads.BaseTypes as BT


import Language.Pads.PadsParser


import Data.Data
import Data.Int 
import qualified Data.ByteString as B  
import Data.Word

--test = parseStringInput trip_parseM "354;;23|53|43(652%%35)4;hefd"


[pads| 

type IntPair a = (BT.Line Int,":",a)

type Line a = (a, "||")


data MyVoid = Myvoid Void



data Trip = Trip (Int, ";;", Lst CharPair, value 34::Int, Foo Int , EOR) 

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

type MyOtherVoid = ()


data May a = Jus a
           | Noth ()


data Exxy a = Exxy {exxy :: Int, Char, aa :: a}
|]




bi::a->(Int,Int_md)->(Char,Char_md)
bi x = undefined
ib y = undefined




