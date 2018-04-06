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

module Arith where
import Language.Pads.Padsc
import qualified Language.Pads.Library.LittleEndian as LE
import qualified Language.Pads.Library.BigEndian as BE
-- import Data.Bits
import qualified Data.ByteString as B
import Data.Word
-- import Language.Pads.Testing
import System.IO.Unsafe (unsafePerformIO)

[pads|
    newtype Image = Image (partition (Header, '\n',
                                      Dimensions, '\n',
                                      [Pixel] terminator EOF) using none)

    type Header = StringC '\n'
    type Header2 = String

    data Dimensions = Dimensions {w :: Int, ' ', h :: Int}

    data Pixel = Pixel {
        a        :: Bits16 9,
        b        :: Bits8 5,
        c        :: Bits8 5,
        d        :: Bits8 5,
        pb_index :: Bits8 4,
        pr_index :: Bits8 4
    }
|]

nc = "data/arith_image"

(Image (header, dimensions, pixels), md) =
    unsafePerformIO $ parseFileWith image_parseM nc

-- test =
--     let p0 = pixels !! 0
--         p1 = pixels !! 100
--         p2 = pixels !! 328
--         p3 = pixels !! 897
--         p4 = pixels !! 1028
--         p5 = pixels !! 3205
--         p6 = pixels !! 5067
--         p7 = pixels !! 7345
--         p8 = pixels !! 9834
--         p9 = pixels !! (length pixels - 1)
--         ps = [p0,p1,p2,p3,p4,p5,p6,p7,p8,p9]
--     in  (map a ps,
--          map b ps,
--          map c ps,
--          map d ps,
--          map pb_index ps,
--          map pr_index ps)

-- chars = do
--     (Image' (_, _, cs), md) <- parseFileWith image'_parseM nc
--     return cs
--
-- word8s = do
--     (Image'' (_, _, ws), md) <- parseFileWith image''_parseM nc
--     return ws

--f x = if x == 255 then (x :: Word8) else (x :: Word16)

-- bits = map (\x -> fromIntegral x :: Integer) pixels
-- (n, _) = foldr (\x z -> (x * (2 ^ snd z) + fst z, snd z + 1)) (0,0) bits

-- (EnumTypes codes, codes_md) =
--     unsafePerformIO $ parseFileWith enumTypes_parseM "data/enums.txt"
