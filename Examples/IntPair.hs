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

type Triple = (Int, (transform Int => Char using <|(bi,ib)|>, Int)) 


newtype Void = Void ()

-- newtype Jib = Jib (StringSE $[a-z]+$)


{-
oldtype Maybe a = Just a
                | Nothing ()
-}
|]




bi::a->(Int,Int_md)->(Char,Char_md)
bi x = undefined
ib y = undefined




{-
[type Trip = (Int, Lst CharPair, Foo Int),
 type Trip_md =
	 (Base_md, (Base_md, Lst_md CharPair_md, Foo_md Base_md)),
 trip_parseM
   = let
	   f_rep x1 x3 x4 = (x1, x3, x4)
	   f_md x1 x2 x3 x4 x5
		 = let
			 m1 = get_md_header x1
			 m2 = get_md_header x2
			 m3 = get_md_header x3
			 m4 = get_md_header x4
			 m5 = get_md_header x5
		   in (mergeBaseMDs [m1, m2, m3, m4, m5], (x1, x3, x4))
	 in
	   (((((return (f_rep, f_md) =@= int_parseM) =@ strLit_parseM ";;")
		=@=
		  lst_parseM charPair_parseM)
	   =@=
		 foo_parseM int_parseM)
	  =@
		eor_parseM),
 type Lst a = [a], type Lst_md a = (Base_md, [a]),
 lst_parseM a__parseM
   = parseListSepLength (charLit_parseM '|') 3 a__parseM,
 data CharPair
	 = Char2 {first :: Char, second :: Char}
	 deriving (Show, Eq, Typeable, Data, Ord),
 type CharPair_md = (Base_md, CharPair_imd),
 data CharPair_imd
	 = Char2_md {first_md :: Base_md, second_md :: Base_md}
	 deriving (Show, Eq, Typeable, Data, Ord),
 charPair_parseM
   = let
	   char2_md x1 x2
		 = let
			 m1 = get_md_header x1
			 m2 = get_md_header x2
		   in (mergeBaseMDs [m1, m2], Char2_md x1 x2)
	 in
	   choiceP
		 [((return (Char2, char2_md) =@= char_parseM) =@= char_parseM)],
 data Foo b
	 = Bazz Int | Zipp b !Int
	 deriving (Read, Eq, Show, Typeable, Data, Ord),
 type Foo_md b = (Base_md, Foo_imd b),
 data Foo_imd b
	 = Bazz_md Base_md | Zipp_md b Base_md
	 deriving (Show, Eq, Typeable, Data, Ord),
 foo_parseM b__parseM
   = let
	   bazz_md x1 = let m1 = get_md_header x1 in (m1, Bazz_md x1)
	   zipp_md x1 x2 x3 x4 x5
		 = let
			 m1 = get_md_header x1
			 m2 = get_md_header x2
			 m3 = get_md_header x3
			 m4 = get_md_header x4
			 m5 = get_md_header x5
		   in (mergeBaseMDs [m1, m2, m3, m4, m5], Zipp_md x2 x4)
	 in
	   choiceP
		 [(return (Bazz, bazz_md) =@= int_parseM),
		  (((((return (Zipp, zipp_md) =@ strLit_parseM "(") =@= b__parseM)
		   =@
			 strLit_parseM "%%")
		  =@=
			int_parseM)
		 =@
		   charLit_parseM ')')],
 data Switcher
	 = Zero | One Lst Int
	 deriving (Read, Eq, Show, Typeable, Data, Ord),
 type Switcher_md = (Base_md, Switcher_imd),
 data Switcher_imd
	 = Zero_md | One_md Lst_md Base_md
	 deriving (Show, Eq, Typeable, Data, Ord),
 switcher_parseM x
   = let
	   zero_md x1 = let m1 = get_md_header x1 in (m1, Zero_md)
	   one_md x1 x2 x3
		 = let
			 m1 = get_md_header x1
			 m2 = get_md_header x2
			 m3 = get_md_header x3
		   in (mergeBaseMDs [m1, m2, m3], One_md x2)
	 in
	   case x of {
		 0 -> (return (Zero, zero_md) =@ strLit_parseM "$")
		 1 -> (((return (One, one_md) =@ charLit_parseM 'd')
			  =@=
				lst_parseM int_parseM)
			 =@
			   strLit_parseM "()") }]
-}