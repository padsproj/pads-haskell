{- LANGUAGE FlexibleContexts #-}

module Rand where

import Language.Pads.Padsc
import System.Random
import Data.Random.Normal
import Data.Int
import Data.Word
import Data.Bits
import qualified Data.ByteString as B
import Control.Monad (replicateM)
import System.IO.Unsafe (unsafePerformIO)

err = error "Bad range"

randInt :: Integral a => a -> a -> Word64
randInt lo hi = if lo > hi then err else if lo == hi then fromIntegral lo else
    -- let xs = unsafePerformIO $ replicateM 64 (randomIO :: IO Word64)
    --     bits = map (`mod` 2) xs
    --     (n,_) = foldr (\x z -> (x * (2 ^ snd z) + fst z, snd z + 1)) (0,0) bits
    let n = unsafePerformIO $ (randomIO :: IO Word64)
        lo' = fromIntegral lo
        hi' = fromIntegral hi
        num = n `mod` (hi' + 1)
    in  if   num < lo'
        then num + randInt (lo' - num) (hi' - num)
        else num

-- randIntM :: Integral a => a -> a -> IO Word64
-- randIntM lo hi = if lo > hi then err else if lo == hi then fromIntegral lo else do
--     n <- (randomIO :: IO Word64)
--     let lo' = fromIntegral lo
--     let hi' = fromIntegral hi
--     let num = n `mod` (hi' + 1)
--     return (if   num < lo'
--             then num + randIntM (lo' - num) (hi' - num)
--             else num)


-- Randoms of specific types

rand8 :: a -> Word8
rand8 _ = unsafePerformIO $ (randomIO :: IO Word8)
rand8' = randomIO :: IO Word8

rand8S :: a -> Int8
rand8S _ = unsafePerformIO $ (randomIO :: IO Int8)
rand8S' = randomIO :: IO Int8

rand16 :: a -> Word16
rand16 _ = unsafePerformIO $ (randomIO :: IO Word16)
rand16' = randomIO :: IO Word16

rand16S :: a -> Int16
rand16S _ = unsafePerformIO $ (randomIO :: IO Int16)
rand16S' = randomIO :: IO Int16

rand32 :: a -> Word32
rand32 _ = unsafePerformIO $ (randomIO :: IO Word32)
rand32' = randomIO :: IO Word32

rand32S :: a -> Int32
rand32S _ = unsafePerformIO $ (randomIO :: IO Int32)
rand32S' = randomIO :: IO Int32

rand64 :: a -> Word64
rand64 _ = unsafePerformIO $ (randomIO :: IO Word64)
rand64' = randomIO :: IO Word64

rand64S :: a -> Int64
rand64S _ = unsafePerformIO $ (randomIO :: IO Int64)
rand64S' = randomIO :: IO Int64

randElem :: [a] -> a
randElem xs = xs !! (fromIntegral $ randInt 0 (length xs - 1))

randChar :: a -> Char
randChar _ = word8ToChr $ rand8 0

thing 0 = []
thing x = randChar 0 : thing (x - 1)


randIntN :: Integral a => Double -> Double -> a -> a -> a
randIntN m s lo hi = if lo > hi then err else if lo == hi then fromIntegral lo else
    let x = unsafePerformIO $ (normalIO' (m, s) :: IO Double)
        x' = if x < m then ceiling x else floor x
    in  if   x' < lo || x' > hi
        then randIntN m s lo hi
        else x'

randIntNList 0 _ _ _ _= []
randIntNList x m s lo hi = (randIntN m s lo hi) : randIntNList (x - 1) m s lo hi


range :: a -> (Word64, Word64)
range _ =
    let xs = unsafePerformIO $ replicateM 1000000 (randomIO :: IO Word64)
    in  foldr (\x (lo, hi) -> (min x lo, max x hi)) (Prelude.head xs, Prelude.head xs) xs

dist m s =
    let ns  = unsafePerformIO $ replicateM 10000 (normalIO' (m, s) :: IO Double)
        integerizeWithMean m x = if x < m then ceiling x else floor x
        ns' = map (integerizeWithMean m) ns
        lo  = foldr min (Prelude.head ns') ns'
        hi  = foldr max (Prelude.head ns') ns'
    in  ( map (\x -> foldr (\y z -> if x == y then z + 1 else z) 0 ns') [lo..hi]
        , lo
        , hi )



randString _ 0 = []
randString cs x = randElem cs : randString cs (x - 1)

randString' 0 = []
randString' x = (word8ToChr $ rand8 0) : randString' (x - 1)

chars = [' '..'~']

--wordToBS :: Integral a => a -> B.ByteString
wordToBS x = B.pack $ reverse $ map fromIntegral (wordToBS' x)

--wordToBS' :: (Integral a, Bits a) => a -> [Word8]
wordToBS' 0 = []
wordToBS' x = (x .&. 255) : wordToBS' (shiftR x 8)

-- dist' m s lo hi =
--     let ns  = (replicate 10 (randIntN m s lo hi))
--         -- ns' = map (integerizeWithMean m) ns
--         lo'  = foldr min (head ns) ns
--         hi'  = foldr max (head ns) ns
--     in  ( map (\x -> foldr (\y z -> if x == y then z + 1 else z) 0 ns) [lo'..hi']
--         , lo'
--         , hi' )


-- randInt :: Num a => Int -> Int -> a
-- randInt lo hi =
--     let intmax = 9223372036854775807
--         xs = unsafePerformIO $ replicateM 100 (randomIO :: IO Int)
--         xs' = (map (\x -> (fromIntegral x) + intmax) xs) -- :: [Integer]
--         r1 = xs' !! ((xs' !! 0) `mod` 100)
--         r2 = xs' !! (r1 `mod` 100)
--         r3 = xs' !! (r2 `mod` 100)
--         r4 = xs' !! (r3 `mod` 100)
--         r5 = xs' !! (r4 `mod` 100)
--         r = r5 `mod` (hi + 1)
--     in  if   r < lo
--         then (r + lo) `mod` (hi + 1)
--         else r


-- randInt lo hi = if lo > hi then err else if lo == hi then fromIntegral lo else
--     let lo' = fromIntegral lo
--         hi' = fromIntegral hi
--         x = unsafePerformIO $ (randomIO :: IO Double)
--         y = x * (hi' - lo' + 1)
--         z = floor (y + lo')
--     in  fromIntegral z
