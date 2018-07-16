{-# LANGUAGE FlexibleContexts #-}

module Language.Pads.DataGen.Generation where

import Control.Monad.Reader hiding (lift)
import Control.Monad.IO.Class
import Data.Word
import System.Random.MWC

-- | A custom ReaderT monad, allowing threaded access to a random number
-- generator, supplied when the generation function is called at top level
-- (i.e. via runGen)
type PadsGen = ReaderT GenIO IO

-- | Provides the requisite random number generator to the supplied generation
-- computation and returns the result as a value in IO.
runGen :: PadsGen a -> IO a
runGen genM = do
  gen <- createSystemRandom
  runReaderT genM gen

-- The type randNum and randNumBound return is dictated by the type signature
-- of whatever function calls them.

randNum :: Variate a => PadsGen a
randNum = ask >>= (liftIO . uniform)

randNumBound :: (Integral a, Variate a) => a -> PadsGen a
randNumBound i = do
  gen <- ask
  liftIO $ uniformR (0, i) gen


-- As they aren't members of the System.Random.MWC class Variate, Integers
-- require special treatment - convert them from doubles.
randInteger :: PadsGen Integer
randInteger = randIntegerBound (2^1023)

randIntegerBound :: Integral a => a -> PadsGen Integer
randIntegerBound i = do
  gen <- ask
  liftIO $ (toInteger . floor) <$> uniformR (0 :: Double, (fromIntegral i) :: Double) gen


randElem :: [a] -> PadsGen a
randElem xs = do
    gen <- ask
    r <- liftIO $ fromIntegral <$> uniformR (0, length xs - 1) gen
    return $ xs !! r

possChars :: [Char]
possChars = ['A'..'Z'] ++ ['a'..'z']

randLetter :: PadsGen Char
randLetter = do
  gen <- ask
  randElem possChars

randLetterExcluding :: Char -> PadsGen Char
randLetterExcluding c = do
    gen <- ask
    char <- randElem possChars
    if (c == char)
        then do { char' <- randLetterExcluding c
                ; return char' }
        else return char
