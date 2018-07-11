module Language.Pads.DataGen.Rand where

import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.IO.Class
import           System.Random
import qualified System.Random.MWC as MWC

type M = ReaderT MWC.GenIO IO

randInt :: M Int
randInt = do
  gen <- ask
  liftIO $ MWC.uniformR (0, 2147483647) gen

randElem :: [a] -> M a
randElem xs = do
    gen <- ask
    r <- liftIO $ fromIntegral <$> MWC.uniformR (0, length xs - 1) gen
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
