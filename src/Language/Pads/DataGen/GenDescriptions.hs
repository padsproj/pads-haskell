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

    newtype Addresses = Addresses [Address]
    data Address = Address { streetnum :: Int,
                             ' ',
                             streetname :: StringC '\n'}

    -- START   -- _ --> S1
    -- S1      -- c --> S2
    -- S2      -- a --> S3
    -- S3      -- a --> S3
    -- S3      -- t --> QACCEPT

    data START = START { S1 }

    data S1 = S1 { 'c', S2 }

    data S2 = S2 { 'a', S3 }

    data S3 = S3A { 'a', S3 }
            | S3T { 't', S4 }

    data S4 = S4 { }

--                                     ___ a ___
--                                   /           \
--  |                               |             |      |--------|
--  |    |----|          |----|      \-> |----| -/       | |----| |
--   \-> | S1 | -- c --> | S2 | -- a --> | S3 | -- t --> | | S4 | |
--       |----|          |----|          |----|          | |----| |
--                                                       |--------|

    data Pixel = Pixel {
        a :: Bits16 9,
        b :: Bits8 5,
        c :: Bits8 5,
        d :: Bits8 5,
        pb_index :: Bits8 4,
        pr_index :: Bits8 4
    }

    type Pixels = partition [Pixel] using none

    data Mixed = Mixed {
        bits1 :: Bits8 4,
        'c',
        bits2 :: Bits8 4
    }

    data Dependent = Dependent { first :: Bits8 4, second :: Bits16 first, onronr::Char }

    -- data Dependent2 = Dependent2 { third :: Bits8 8, fourth :: Bytes third }

    data Test0 = Test0 { f :: Bits8 6
                       , g :: Test1 f }
    data Test1 (x :: Bits8) = Test1 { h :: Bits64 x }

    data DT = DT1 [Int | ',']
            | DT2 " START DT2 " DT "||" DT " END DT2 "

    type SomeBytes = Bytes 8

    data PadsTyValue = PadsTyValue { ptv :: Int, xIsEven = value <| even ptv |> :: Bool }

    -- All strings with an equal number of 01 and 10 substrings:

    data Q1 = Q1_1 { '1', Q2 }
            | Q1_0 { '0', Q4 }
            | Q1_A {}
    data Q2 = Q2_1 { '1', Q2 }
            | Q2_0 { '0', Q3 }
            | Q2_A {}
    data Q3 = Q3_1 { '1', Q2 }
            | Q3_0 { '0', Q3 }
    data Q4 = Q4_1 { '1', Q5 }
            | Q4_0 { '0', Q4 }
            | Q4_A {}
    data Q5 = Q5_1 { '1', Q5 }
            | Q5_0 { '0', Q4 }

    type ThreeInts = (Int, Char, Int)
|]




-- testdfa (mk_render_char ast_START)
