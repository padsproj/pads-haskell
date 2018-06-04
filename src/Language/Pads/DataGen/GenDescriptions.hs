{-# LANGUAGE TypeFamilies
           , ScopedTypeVariables
           , DeriveDataTypeable
           , MultiParamTypeClasses
           , TypeSynonymInstances
           , TemplateHaskell
           , QuasiQuotes
           , FlexibleInstances
           , FlexibleContexts
           , RecordWildCards
           , NamedFieldPuns #-}



module Language.Pads.DataGen.GenDescriptions where

import Language.Pads.Padsc





--import qualified Language.Pads.Rand as RN
--import qualified Data.ByteString.Conversion as BC


padsSamples = [pads|

    data Simple = Simple {
        c1 :: Char,
        '|',
        "constant string",
        '|',
        i1 :: Int,
        '|',
        r1 :: StringFW 20
    }

    data Name = Bob "Bob" | Fred "Fred"

    data Record = Record {
        "first:",
        fname :: Name, ' ',
        "middle:",
        mname :: Name, ' ',
        "last:",
        lname :: Name, ' ',
        "age:",
        age :: Int, ' ',
        "height:",
        height :: Int
    }

    data Records = Records [Record | " | "]



    -- START   -- _ --> S1
    -- S1      -- c --> S2
    -- S2      -- a --> S3
    -- S3      -- a --> S3
    -- S3      -- t --> ACCEPT


    data START = START {start::S1}

    -- s1 -- c --> s2

    data S1 = S1 {
                    'c',
                     s2::S2
                 }

    -- s2 -- a --> s3

    data S2 = S2 {  'a',
                    s3 :: S3
                 }

    -- s3 -- a --> s3
    -- s3 -- t --> s4

    data S3 = S3A {'a', s3a::S3 } |
              S3T {'t', ss4::S4 }

    data S4 = S4 {s4::ACCEPT}

    data ACCEPT = ACCEPT { }



|]




-- testdfa (mk_render_char ast_START)
