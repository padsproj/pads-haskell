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
           , NamedFieldPuns
           , OverloadedStrings #-}

module DFA where

import Language.Pads.Padsc
import Language.Pads.Testing
import Data.Word




[pads|
    -- s1 -- c --> s2
    -- s2 -- a --> s3
    -- s3 -- a --> s3
    -- s3 -- t --> s4

    data START = START {start::S1}

    data S1 = S1 {
                    'c',
                     s2::S2 
                 }

    data S2 = S2 {  'a',
                    s3 :: S3
                 }

    data S3 = S3A { 'a', s3a::S3 } |
              S3T { 't', ss4::S4 }

    data S4 = S4 {s4::ACCEPT}

    data ACCEPT = ACCEPT ()

|]




testdfa parser str = 
  let 
    ((x, (y, z)), s) = parser str
    bad = numErrors y + length s       
  in case bad of 
    0 -> "DFA accepts " ++ str ++ ""
    n -> "DFA rejects " ++ str ++ " with " ++ (show n) ++ " errors."

--testdfa sTART_parseS "caaat"
--testdfa sTART_parseS "cat"
--testdfa sTART_parseS "dog"
