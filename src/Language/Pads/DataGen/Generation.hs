module Language.Pads.DataGen.Generation (
    module Control.Monad.Reader,
    module Control.Monad.IO.Class,
    module System.Random.MWC,
    M, run, randInt, randElem, possChars, randLetter, randLetterExcluding
    ) where

import Control.Monad.Reader hiding (lift)
import Control.Monad.IO.Class
import System.Random.MWC

type M = ReaderT GenIO IO

run :: M a -> IO a
run x = do
  gen <- createSystemRandom
  runReaderT x gen

randInt :: M Int
randInt = do
  gen <- ask
  liftIO $ uniformR (0, 2147483647) gen

randElem :: [a] -> M a
randElem xs = do
    gen <- ask
    r <- liftIO $ fromIntegral <$> uniformR (0, length xs - 1) gen
    return $ xs !! r

possChars :: [Char]
possChars = ['a'..'z']

randLetter :: M Char
randLetter = do
  gen <- ask
  randElem possChars

randLetterExcluding :: Char -> M Char
randLetterExcluding c = do
    gen <- ask
    char <- randElem possChars
    if (c == char)
        then do { char' <- randLetterExcluding c
                ; return char' }
        else return char
