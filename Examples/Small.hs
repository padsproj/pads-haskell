{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}
module Examples.Small where
import Language.Pads.Padsc

[pads| type StringFWs = [StringFW 3| EOR] terminator EOF |]
stringFWs_input = "abc\nabcd\nab\nabc"
stringFWs_result = stringFWs_parseS stringFWs_input
stringFWs_expects = (["abc","abc","XXX","abc"],2,"")

{-
The first error has the wrong line number; the second error has the correct one.
((["abc","abc","XXX","abc"],(Errors: 2 Extra bytes: d before seperator. at: Line: 3, Offset: 0,[Errors: 0 ,Errors: 1 Extra bytes: d before seperator. at: Line: 3, Offset: 0,Errors: 1 Found 2 bytes when looking for 3bytes. at: Line: 3, Offset: 2,Errors: 0 ])),"")
-}



