{- LANGUAGE FlexibleContexts #-}

module Rand where

import Language.Pads.Padsc
import System.Random
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as Dist
import Data.Int
import Data.Word
import Data.Bits
import qualified Data.ByteString as B
import Control.Monad (replicateM)
import System.IO.Unsafe (unsafePerformIO)

normalIO' :: (Double, Double) -> IO Double
normalIO' (mean, sigma) = MWC.withSystemRandom . MWC.asGenST $ Dist.normal mean sigma

err = error "Bad range"

randInt :: Int -> Int -> MWC.GenIO -> IO Int
randInt lo hi gen = MWC.uniformR (lo, hi) gen


-- Randoms of specific types

rand8 :: MWC.GenIO -> IO Word8
rand8 gen = fromIntegral <$> MWC.uniformR (0::Word8, 255::Word8) gen :: IO Word8

randElem :: [a] -> MWC.GenIO -> IO a
randElem xs gen = do
    r <- fromIntegral <$> randInt 0 (length xs - 1) gen
  --r <- fmap fromIntegral (randInt 0 (length xs - 1) gen)
    return $ xs !! r
--randElem xs = xs !! (fromIntegral $ randInt 0 (length xs - 1))

randChar :: MWC.GenIO -> IO Char
randChar gen = word8ToChr <$> rand8 gen
