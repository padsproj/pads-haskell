{-# LANGUAGE TypeFamilies, TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}
module Examples.Small where
import Language.Pads.Padsc

[pads| type StringFWs = [StringFW 3| EOR] terminator EOF |]
stringFWs_input = "abc\nabcd\nab\nabc"
stringFWs_result = stringFWs_parseS stringFWs_input
stringFWs_expects = (["abc","abc","XXX","abc"],2,"")

{-
The first error has the wrong line number; the second error has the correct one.
((["abc","abc","XXX","abc"],(Errors: 2 Extra bytes: d before seperator. at: Line: 3, Offset: 0,[Errors: 0 ,Errors: 1 Extra bytes: d before seperator. at: Line: 3, Offset: 0,Errors: 1 Found 2 bytes when looking for 3bytes. at: Line: 3, Offset: 2,Errors: 0 ])),"")

Also, test arrays with nullable element types.  Does this cause an infinite loop?

-}
typeRE = REd "TYPE|type" "TYPE"

[pads| data EmailType = EmailInternet "INTERNET"
               | EmailX400  "X400"
               | EmailPreferred (StringME 'PREF|pref')
               | EmailWork "WORK"
       type CommaL a = [a|','] terminator Try (Lit ";") 
       type TypeL  a = [(typeRE, '=', CommaL a) | ';'] terminator ':'
       type EList = TypeL EmailType 

|]

commaL_input = "type=INTERNET;type=WORK;type=pref:"
commaL_result = eList_parseS commaL_input



