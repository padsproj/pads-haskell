module Language.Pads.DataGen.Rand where

import           System.Random
import qualified System.Random.MWC as MWC

randInt :: MWC.GenIO -> IO Int
randInt gen = MWC.uniformR (0, 2147483647) gen

randElem :: [a] -> MWC.GenIO -> IO a
randElem xs gen = do
    r <- fromIntegral <$> MWC.uniformR (0, length xs - 1) gen
    return $ xs !! r

possChars :: [Char]
possChars = ['a'..'z']

randLetter :: MWC.GenIO -> IO Char
randLetter gen = randElem possChars gen

randLetterExcluding :: Char -> MWC.GenIO -> IO Char
randLetterExcluding c gen = do
    char <- randElem possChars gen
    if (c == char)
        then do { char' <- randLetterExcluding c gen
                ; return char' }
        else return char
