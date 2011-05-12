{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, NamedFieldPuns, ScopedTypeVariables #-}

module Examples.AI where
import Language.Pads.Padsc hiding (str)
import Language.Pads.GenPretty       
import Control.Monad

import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString.Lazy.Char8 as B

[pads|
  newtype AI    = AI ([Line Entry] terminator EOF)

  data Entry = Entry
      {     host       :: Src, 
      ' ',  identdID   :: ID, 
      ' ',  httpID     :: ID, 
      ' ',  time       :: TimeStamp, 
      ' ',  request    :: Request,
      ' ',  response   :: Response,
      ' ',  contentLen :: ContentLength }
      
  data Src = Addr IP_v4 | Name Host  
  type IP_v4 = (IPInt, '.', IPInt, '.', IPInt, '.', IPInt)
  type Host = StringC ' '
  type IPInt = constrain i :: Int where <| 0 <= i && i < 256 |>   

  data ID = Missing '-' | Id (StringC ' ')

  type TimeStamp = ('[', Date, ':', Time, ' ', TimeZone, ']')   

  data Date = Date {day::Day, '/', month::Month, '/', year::Int}    
  data Time = Time {hours::Hours, ':', minutes :: Minutes, ':', seconds :: Seconds}  
  type TimeZone = ('-', Int)

  type Year    = constrain y :: Int where <| 1900 <= y && y < 3000 |>   
  data Month   = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec  
  type Day     = constrain d :: Int where <| 1 <= d && d < 31 |>   
  type Hours   = constrain h :: Int where <| 0 <= h && h < 24 |>   
  type Minutes = constrain m :: Int where <| 0 <= m && m < 60 |> 
  type Seconds = constrain s :: Int where <| 0 <= s && s < 60 |> 

  data Request = Request 
      { '"',  method  :: Method,       
        ' ',  url     :: StringC ' ', 
        ' ',  version :: Version  where <| checkVersion method version |>,  '"'
      }  

  data Method  = GET | PUT | POST | HEAD | DELETE
               | LINK | UNLINK      -- obsolete after http 1.0
  data Version = Version {"HTTP/", major :: Int, '.', minor :: Int}

  type Response = constrain r :: Int where <| 100 <= r && r < 600 |> 

  data ContentLength = NotAvailable '-' | ContentLength Int 
  |]

checkVersion :: Method -> Version -> Bool
checkVersion method version = 
  case method of
    LINK   -> major version == 1 && minor version == 0
    UNLINK -> major version == 1 && minor version == 0
    _ -> True


mkPrettyInstance ''AI
--mkPrettyInstance ''AI_md

ai_file = ai_big
ai_3000 = "Examples/data/ai.3000"
ai_big  = "Examples/data/ai.big"

-- (ai_rep, ai_md) = let (AI rep, md) = unsafePerformIO $ parseFile ai_file in (rep,md)
(AI ai_rep, ai_md) =  unsafePerformIO $ parseFileWith aI_parseM ai_file


ai_file_length  = Prelude.length ai_rep
ai_file_take n  = Prelude.take n ai_rep

printAI n = putStrLn(pretty 100 (ppr (ai_file_take n)))

{-
result n  = do 
     { (AI rep, md) <- parseFile ai_file
     ; return (Prelude.take n ai_rep)
     } 
-}


