{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, NamedFieldPuns, ScopedTypeVariables #-}

module Examples.AI where
import Language.Pads.Padsc hiding (str)
import Language.Pads.GenPretty       
import Control.Monad

import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString.Lazy.Char8 as B

[pads|
  type AI    = [Line Entry] with term Eof

  data Entry = {     host       :: Src, 
               ' ',  identdID   :: ID, 
               ' ',  httpID     :: ID, 
               ' ',  time       :: TimeStamp, 
               ' ',  request    :: Request,
               ' ',  response   :: Response,
               ' ',  contentLen :: ContentLength }
      

  data Src = Addr IP | Name Host  
  type IP = (Pint, '.', Pint, '.', Pint, '.', Pint)
  type Host = Pstring ' '


  data ID = Missing '-' | Id (Pstring ' ')


  type TimeStamp = ('[', Date, ':', Time, ' ', TimeZone, ']')   
  data Date = {day::Pint, '/', month::Month, '/', year::Pint}    
  data Time = {hours::Hours, ':', minutes :: Minutes, ':', seconds :: Seconds}  
  type TimeZone = ('-', Pint)

  data Month   = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec  
  type Hours   = constrain h :: Pint where <| 0 <= h && h < 24 |>   
  type Minutes = constrain m :: Pint where <| 0 <= m && m < 60 |> 
  type Seconds = constrain s :: Pint where <| 0 <= s && s < 60 |> 


  data Request = { '"',  method  :: Method,       
                   ' ',  url     :: Pstring ' ', 
                   ' ',  version :: Version  where <| checkVersion method version |>,  '"'
                    }  
  data Method  = GET | PUT | POST | HEAD | DELETE
                 | LINK | UNLINK      -- obsolete after http 1.0
  type Version = {"HTTP/", major :: Pint, '.', minor :: Pint}
  |]

checkVersion :: Method -> Version -> Bool
checkVersion method version = 
  case method of
    LINK   -> major version == 1 && minor version == 0
    UNLINK -> major version == 1 && minor version == 0
    _ -> True

[pads|
  type Response = constrain r :: Pint where <| 100 <= r && r < 600 |> 

  data ContentLength = NotAvailable '-' | ContentLength Pint 
  |]


mkPrettyInstance ''AI
mkPrettyInstance ''AI_md

ai_file = ai_big
ai_3000 = "Examples/data/ai.3000"
ai_big  = "Examples/data/ai.big"

(ai_rep, ai_md) = let (AI rep, md) = unsafePerformIO $ parseFile ai_file in (rep,md)

ai_file_length  = Prelude.length ai_rep
ai_file_take n  = Prelude.take n ai_rep

result n  = do 
     { (AI rep, md) <- parseFile ai_file
     ; return (Prelude.take n ai_rep)
     } 


[pads|

  |]

example = Prelude.take 2 $ fst $ parseStringInput (parseMany pdigit_parseM) str

str = "1234cnbdav duisc djnklcndjkalscnj dkxbvc daseasklfhasdjkhfaksjdhflakjsdhfkjlahsdfkljahsdlfkhasdkjfhaklsjdhflkashdfjkhjmzb"++ undefined
example2 =  (padsSourceFromString ("abc\nd" ++ undefined))
example3 = B.pack str

first5 = AI (ai_file_take 5)
first5_doc = aI_ppr first5
output n = pretty n first5_doc

