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

module DFAC where
import Language.Pads.Padsc
import Language.Pads.Testing
import Data.Word



[pads|
    
    -- constrained dfa 
    -- accepts all caaats with each character followed by a consistent number:
    -- c1a1t1 or c5a5a5a5t5 but not c1a2t2


    -- START   -- _ --> S1
    -- S1      -- c --> S2
    -- S2      -- a --> S3
    -- S3      -- a --> S3
    -- S3      -- t --> ACCEPT


    data AA = AA {'a','a','a'}

    data START = START {start::S1}

    -- s1 -- c --> s2

    data S1 = S1 {
                    'c',
                     c1 :: Char,
                     s2 :: S2 c1
                 }

    -- s2 -- a --> s3

    data S2 (n::Char) = 
              S2 {  
                    'a',
                    constrain c2 :: Char where <| n == c2 |>,
                              s3 :: S3 c2
                 }

    -- s3 -- a --> s3
    -- s3 -- t --> s4

    data S3 (n::Char) = 
              S3A {  'a', 
                    constrain c3a :: Char where <| n == c3a |>,
                              s3a :: S3 c3a 
                  } |
              S3T { 't', 
                    constrain c3t :: Char where <| n == c3t |>,
                              ss4 :: S4 c3t}

    data S4 (n::Char) = S4 {s4::ACCEPT}

    data ACCEPT = ACCEPT ()

|]


testdfa parser str = 
  let 
    ((x, (y, z)), s) = parser str
    bad = numErrors y + length s       
  in case bad of 
    0 -> "DFA accepts " ++ str ++ ""
    n -> "DFA rejects " ++ str ++ " with " ++ (show n) ++ " errors."

--testdfa sTART_parseS "c1a1t1"
--testdfa sTART_parseS "c1a1t2"

--testdfa sTART_parseS "c1a1a1a1a1a1a1a1t2"
--testdfa sTART_parseS "c1a1a1a1a1a1a1a1t1"