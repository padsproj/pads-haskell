{-# LANGUAGE TypeFamilies, ScopedTypeVariables, DeriveDataTypeable
           , MultiParamTypeClasses
           , TypeSynonymInstances
           , TemplateHaskell
           , QuasiQuotes
           , FlexibleInstances #-}

-- :set -ddump-splices

module AI where
import Language.Pads.Padsc
import Language.Pads.GenPretty

import System.IO.Unsafe (unsafePerformIO)


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

  data Src = Addr
      { a :: IPInt, '.'
      , b :: IPInt, '.'
      , c :: IPInt, '.'
      , d :: IPInt
      }
    | Name Host
  --type IP_v4 = (IPInt, '.', IPInt, '.', IPInt, '.', IPInt)
  type IPInt = constrain i :: Int where <| 0 <= i && i < 256 |>
  type Host = StringC ' '

  data ID = Missing '-' | Id (StringC ' ')



  type TimeStamp = ('[', Date, ']')
  type Date = DateFC <|("%d/%h/%Y:%H:%M:%S %z", ']')|>

  data Request = Request
      { '"',  method  :: Method,
        ' ',  url     :: StringC ' ',
        ' ',  version :: Version  where <| checkVersion method version |>,  '"'
      }

  data Method  = GET | PUT | POST | HEAD | DELETE
               | LINK | UNLINK      -- obsolete after http 1.0
  data Version = Version {"HTTP/", major :: Int, '.', minor :: Int}  -- add constriants on major and minor mode

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
mkPrettyInstance ''AI_md

ai_file = ai_big
ai_3000 = "examples/data/ai.3000"
ai_big  = "examples/data/ai.big"

-- (ai_rep, ai_md) = let (AI rep, md) = unsafePerformIO $ parseFile ai_file in (rep,md)
(AI ai_rep, ai_md) =  unsafePerformIO $ parseFileWith aI_parseM ai_file


ai_file_length  = Prelude.length ai_rep
ai_file_take n  = Prelude.take n ai_rep
test = ai_file_take 20

printAI n = putStrLn(pretty 100 (ppr (ai_file_take n)))

result n  = do
     { (AI rep, md) <- parseFile ai_file
     ; return (Prelude.take n rep)
     }
