{-# LANGUAGE NamedFieldPuns,RecordWildCards #-}

module Regression where

import Text.PrettyPrint.Mainland.Class
import Text.PrettyPrint.Mainland as PP
import Language.Pads.Syntax
import qualified Language.Pads.Parser as P
import Language.Pads.RegExp

import Text.Parsec.Error  
import Language.Pads.Pretty



testParsePadsDecls :: String -> Either ParseError [PadsDecl]
testParsePadsDecls input = P.parsePadsDecls "test" 0 0 input 

ppDeclList decls = stack (map ppr decls)

results
  = [result1, result2, result3, result4, result5, result6, result7,
     result8, result9, result10, result11, result12, result13, result14,
     result15, result16, result17, result18, result19, result20, result21,
     result22, result23, result24, result25, result26, result27, result28,
     result29, result30, result31, result32, result33, result34, result35,
     result36, result37, result38, result39, result40, result41, result42,
     result43, result44, result45, result46, result47, result48, result49,
     result50, result51, result52, result53
    ]
result = and results
failures = [n | (r,n) <- zip results [1..], not r]

---------------------


t1 = "type MyChar = Char"
pt1 = testParsePadsDecls t1
ppt1 = case pt1 of Left e -> show e ; Right r -> pretty 80 (ppDeclList r)
result1 = t1 == ppt1

t2 = "type IntPair = (Int, '|', Int)"
pt2 = testParsePadsDecls t2
ppt2 = case pt2 of Left e -> show e ; Right r -> pretty 80 (ppDeclList r)
result2 = t2 == ppt2


t3 = "type Bar = (Int, ',', IntPair, ';', Int)"
pt3 = testParsePadsDecls t3
ppt3 = case pt3 of Left e -> show e ; Right r -> pretty 80 (ppDeclList r)
result3 = t3 == ppt3

t4 = "type Bar2 = (Int, ',', (Int, ':', Int), ';', Int)"
pt4 = testParsePadsDecls t4
ppt4 = case pt4 of Left e -> show e ; Right r -> pretty 80 (ppDeclList r)
result4 = t4 == ppt4

t5 = "type BazR = Line (Int, ',', Int)"
pt5 = testParsePadsDecls t5
ppt5 = case pt5 of Left e -> show e ; Right r -> pretty 80 (ppDeclList r)
result5 = t5 == ppt5


t6 = "type StrTy = StringFW <|testStrLen + computeLen 4|>"
pt6 = testParsePadsDecls t6
ppt6 = case pt6 of Left e -> show e ; Right r -> pretty 80 (ppDeclList r)
result6 = t6 == ppt6

t7 = "type StrTy1 = StringC 'o'"
pt7 = testParsePadsDecls t7
ppt7 = case pt7 of Left e -> show e ; Right r -> pretty 80 (ppDeclList r)
result7 = t7 == ppt7

t8 = "type Baz = (StringFW 3, ',', Int)"
pt8 = testParsePadsDecls t8
ppt8 = case pt8 of Left e -> show e ; Right r -> pretty 80 (ppDeclList r)
result8 = t8 == ppt8

t9 = "type StrME = StringME \"a+\""
pt9 = testParsePadsDecls t9
ppt9 = case pt9 of Left e -> show e ; Right r -> pretty 80 (ppDeclList r)
result9 = t9 == ppt9

t10 = "type StrP1 (x :: Int) = StringFW <|x - 1|>"
pt10 = testParsePadsDecls t10
ppt10 = case pt10 of Left e -> show e ; Right r -> pretty 80 (ppDeclList r)
result10 = t10 == ppt10

t11 = "newtype IntRange = IntRange (constrain x :: Int where <|(0 <= x) && (x <= 256)|>)"
pt11 = testParsePadsDecls t11
ppt11 = case pt11 of Left e -> show e ; Right r -> pretty 80 (ppDeclList r)
result11 = t11 == ppt11

t12 = "type IntRangeP (low :: Int, high :: Int) = constrain x :: Int where <|(low <= x) && ((x <= high) && (numErrors x_md == 0))|>"
pt12 = testParsePadsDecls t12
ppt12 = case pt12 of Left e -> show e ; Right r -> pretty 80 (ppDeclList r)
result12 = t12 == ppt12

t13 = "data Record (bound :: Int) = Rec {i1 :: Int, ',', i2 :: Int} where <|(i1 + i2) <= bound|>"
pt13 = testParsePadsDecls t13
ppt13 = case pt13 of Left e -> show e ; Right r -> pretty 100 (ppDeclList r)
result13 = t13 == ppt13

t14 = "data Record2 (bound :: Int) = Rec2 {i1 :: Int, ',', i2 :: Int where <|(i1 + i2) <= bound|>}"
pt14 = testParsePadsDecls t14
ppt14 = case pt14 of Left e -> show e ; Right r -> pretty 100 (ppDeclList r)
result14 = t14 == ppt14

t15 = "data Id = Numeric Int | Alpha (StringC ',')"
pt15 = testParsePadsDecls t15
ppt15 = case pt15 of Left e -> show e ; Right r -> pretty 100 (ppDeclList r)
result15 = t15 == ppt15

t16 = "data Id3 = Numeric3 (IntRangeP <|(1, 10)|>) | Numeric3a Int | Lit3 ','"
pt16 = testParsePadsDecls t16
ppt16 = case pt16 of Left e -> show e ; Right r -> pretty 100 (ppDeclList r)
result16 = t16 == ppt16

t17 = "data Ab_or_a = AB \"ab\" | A \"a\""
pt17 = testParsePadsDecls t17
ppt17 = case pt17 of Left e -> show e ; Right r -> pretty 100 (ppDeclList r)
result17 = t17 == ppt17

t18 = "data AB_test = AB_test {field_AB :: Ab_or_a, 'b'}"
pt18 = testParsePadsDecls t18
ppt18 = case pt18 of Left e -> show e ; Right r -> pretty 100 (ppDeclList r)
result18 = t18 == ppt18

t19 = "data Method = GET | PUT | LINK | UNLINK | POST"
pt19 = testParsePadsDecls t19
ppt19 = case pt19 of Left e -> show e ; Right r -> pretty 100 (ppDeclList r)
result19 = t19 == ppt19

t20 = "data Version = Version {\"HTTP/\", major :: Int, '.', minor :: Int}"
pt20 = testParsePadsDecls t20
ppt20 = case pt20 of Left e -> show e ; Right r -> pretty 100 (ppDeclList r)
result20 = t20 == ppt20

t21 = "data Request = Request {'\"', method :: Method, ' ', url :: StringC ' ', ' ', version :: Version where <|checkVersion method version|>, '\"'}"
pt21 = testParsePadsDecls t21
ppt21 = case pt21 of Left e -> show e ; Right r -> pretty 150 (ppDeclList r)
result21 = t21 == ppt21

t22 = "type Eor_Test = (Int, Eor, Int)"
pt22 = testParsePadsDecls t22
ppt22 = case pt22 of Left e -> show e ; Right r -> pretty 100 (ppDeclList r)
result22 = t22 == ppt22

t23 = "type Eof_Test = (Int, Eor, Int, Eof)"
pt23 = testParsePadsDecls t23
ppt23 = case pt23 of Left e -> show e ; Right r -> pretty 100 (ppDeclList r)
result23 = t23 == ppt23

t24 = "type Opt_test = (Int, '|', Maybe Int, '|', Int)" 
pt24 = testParsePadsDecls t24
ppt24 = case pt24 of Left e -> show e ; Right r -> pretty 100 (ppDeclList r)
result24 = t24 == ppt24

t25 = "type Entries_nosep_noterm = [StringFW 3]"
pt25 = testParsePadsDecls t25
ppt25 = case pt25 of Left e -> show e ; Right r -> pretty 100 (ppDeclList r)
result25 = t25 == ppt25

t26 = "type Entries_nosep_noterm2 = [Char]"
pt26 = testParsePadsDecls t26
ppt26 = case pt26 of Left e -> show e ; Right r -> pretty 100 (ppDeclList r)
result26 = t26 == ppt26

t27 = "type EvenInt = constrain x :: Digit where <|(x `mod` 2) == 0|>"
pt27 = testParsePadsDecls t27
ppt27 = case pt27 of Left e -> show e ; Right r -> pretty 100 (ppDeclList r)
result27 = t27 == ppt27

t28 = "type DigitList = [Digit | ',']"
pt28 = testParsePadsDecls t28
ppt28 = case pt28 of Left e -> show e ; Right r -> pretty 100 (ppDeclList r)
result28 = t28 == ppt28

t29 = "type DigitListLen (x :: Int) = [Digit] length <|x + 1|>"
pt29 = testParsePadsDecls t29
ppt29 = case pt29 of Left e -> show e ; Right r -> pretty 100 (ppDeclList r)
result29 = t29 == ppt29

t30 = "type DigitListLenSep (x :: Int) = [Digit | \"ab\"] length <|x + 1|>"
pt30 = testParsePadsDecls t30
ppt30 = case pt30 of Left e -> show e ; Right r -> pretty 100 (ppDeclList r)
result30 = t30 == ppt30

t31 = "type DigitListTerm = [Digit] terminator Eor"
pt31 = testParsePadsDecls t31
ppt31 = case pt31 of Left e -> show e ; Right r -> pretty 100 (ppDeclList r)
result31 = t31 == ppt31

t32 = "type DigitListTermSep = [Digit | '|'] terminator ';'"
pt32 = testParsePadsDecls t32
ppt32 = case pt32 of Left e -> show e ; Right r -> pretty 100 (ppDeclList r)
result32 = t32 == ppt32

t33 = "type TryTest = (Try Char, StringFW 3)"
pt33 = testParsePadsDecls t33
ppt33 = case pt33 of Left e -> show e ; Right r -> pretty 100 (ppDeclList r)
result33 = t33 == ppt33

t34 =    "type ListWithTry = ([Char] terminator Try Digit, Digit)"
pt34 = testParsePadsDecls t34
ppt34 = case pt34 of Left e -> show e ; Right r -> pretty 100 (ppDeclList r)
result34 = t34 == ppt34

t35 = "type WithVoid = (Char, ',', Void, '|')"
pt35 = testParsePadsDecls t35
ppt35 = case pt35 of Left e -> show e ; Right r -> pretty 100 (ppDeclList r)
result35 = t35 == ppt35

t36 = "data VoidOpt = PDigit Digit | Pcolor \"red\" | Pnothing Void"
pt36 = testParsePadsDecls t36
ppt36 = case pt36 of Left e -> show e ; Right r -> pretty 100 (ppDeclList r)
result36 = t36 == ppt36

t37 = "type VoidEntry = (VoidOpt, StringFW 3)"
pt37 = testParsePadsDecls t37
ppt37 = case pt37 of Left e -> show e ; Right r -> pretty 100 (ppDeclList r)
result37 = t37 == ppt37

t38 = "data Switch (which :: Int) = case which of 0 -> Even Int where <|(even `mod` 2) == 0|> | 1 -> Comma ',' | otherwise -> Missing Void"
pt38 = testParsePadsDecls t38
ppt38 = case pt38 of Left e -> show e ; Right r -> pretty 150 (ppDeclList r)
result38 = t38 == ppt38

t39 = "data MyBody (which :: Int) = case which of 0 -> First Int | 1 -> Second (StringC ',') | otherwise -> Other Void"
pt39 = testParsePadsDecls t39
ppt39 = case pt39 of Left e -> show e ; Right r -> pretty 150 (ppDeclList r)
result39 = t39 == ppt39

t40 = "data MyEntry = MyEntry {header :: Int, ',', body :: MyBody header, ',', trailer :: Char}"
pt40 = testParsePadsDecls t40
ppt40 = case pt40 of Left e -> show e ; Right r -> pretty 150 (ppDeclList r)
result40 = t40 == ppt40

t41 = "type MyData = [Line MyEntry] terminator Eof"
pt41 = testParsePadsDecls t41
ppt41 = case pt41 of Left e -> show e ; Right r -> pretty 150 (ppDeclList r)
result41 = t41 == ppt41

t42 = "data HP = HP {student_num :: Int, ',', student_name :: StringFW <|pintToInt student_num|>}"
pt42 = testParsePadsDecls t42
ppt42 = case pt42 of Left e -> show e ; Right r -> pretty 150 (ppDeclList r)
result42 = t42 == ppt42

t43 = "type HP_data = [Line HP]"
pt43 = testParsePadsDecls t43
ppt43 = case pt43 of Left e -> show e ; Right r -> pretty 150 (ppDeclList r)
result43 = t43 == ppt43

acomma = ','
t44 = "data LitRec = LitRec {fstField :: Int, acomma, sndField :: Int}"
--      "data LitRec = LitRec {fstField :: Int, acomma :: Int, sndField :: Int}"
pt44 = testParsePadsDecls t44
-- Urgh. Parses as multiple Int fields.
ppt44 = case pt44 of Left e -> show e ; Right r -> pretty 150 (ppDeclList r)
result44 = t44 == ppt44

t45 = "type WhiteSpace = (Int, '[ \\t]+', Int)"
pt45 = testParsePadsDecls t45
ppt45 = case pt45 of Left e -> show e ; Right r -> pretty 150 (ppDeclList r)
result45 = t45 == ppt45

ws = RE "[ \t]+"
t46 = "type WhiteSpace2 = (Int, ws, Int)"
pt46 = testParsePadsDecls t46
ppt46 = case pt46 of Left e -> show e ; Right r -> pretty 150 (ppDeclList r)
result46 = t46 == ppt46

t47 = "type RE_ty = (Pre \"[tod]\", ws, Pre \"a+\")"
pt47 = testParsePadsDecls t47
ppt47 = case pt47 of Left e -> show e ; Right r -> pretty 150 (ppDeclList r)
result47 = t47 == ppt47

t48 = "type Pstringln = Line (constrain x :: PstringSE \"$\" where True)"
pt48 = testParsePadsDecls t48
ppt48 = case pt48 of Left e -> show e ; Right r -> pretty 150 (ppDeclList r)
result48 = t48 == ppt48

t49 = "type Phex32FW (size :: Int) = transform StringFW size => Int using <|(hexStr2Int, int2HexStr size)|>"
pt49 = testParsePadsDecls t49
ppt49 = case pt49 of Left e -> show e ; Right r -> pretty 150 (ppDeclList r)
result49 = t49 == ppt49

t50 = "type Int = transform StringME \"[0..9]+\" => Int using <|(s2i, i2s)|>"
pt50 = testParsePadsDecls t50
ppt50 = case pt50 of Left e -> show e ; Right r -> pretty 150 (ppDeclList r)
result50 = t50 == ppt50

t51 = "type Char = transform StringFW 1 => Char using <|(head, \\x -> [x])|>"
pt51 = testParsePadsDecls t51
ppt51 = case pt51 of Left e -> show e ; Right r -> pretty 151 (ppDeclList r)
result51 = t51 == ppt51

t52  = "type Fun a b = transform b => (a -> b) using <|(const, \\f -> f def)|>"
t52' = "type Fun a b = transform b => (->) a b using <|(const, \\f -> f def)|>"
pt52 = testParsePadsDecls t52
ppt52 = case pt52 of Left e -> show e ; Right r -> pretty 152 (ppDeclList r)
result52 = t52' == ppt52

t53   = "type StrTy = PstringFW <|testStrLen + computeLen 4|>"
pt53  = testParsePadsDecls t53
ppt53 = case pt53 of Left e -> show e ; Right r -> pretty 150 (ppDeclList r)
result53 = t53 == ppt53

t54   = "data HP = HP {student_num :: Pint, ',', student_name :: PstringFW <|pintToInt student_num|>}\ntype HP_data = [Line HP]"
pt54  = testParsePadsDecls t54
ppt54 = case pt54 of Left e -> show e ; Right r -> pretty 250 (ppDeclList r)
result54 = t54 == ppt54

(t55,pt55,ppt55,result55) = (t,pt,ppt,result)
  where
    t      = "newtype Void = Void ()"
    pt     = testParsePadsDecls t
    ppt    = case pt of Left e -> show e ; Right r -> pretty 150 (ppDeclList r)
    result = t == ppt
       
(t56,pt56,ppt56,result56) = (t,pt,ppt,result)
  where
    t      = "type Foo = Baz '[a-z+]'"
    pt     = testParsePadsDecls t
    ppt    = case pt of Left e -> show e ; Right r -> pretty 150 (ppDeclList r)
    result = t == ppt

(t57,pt57,ppt57,result57) = (t,pt,ppt,result)
  where
    t      = "type BazR = Line (Pint, ',', Pint)"
    pt     = testParsePadsDecls t
    ppt    = case pt of Left e -> show e ; Right r -> pretty 150 (ppDeclList r)
    result = t == ppt



