{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Pads.Generation where

import Control.Monad.Reader hiding (lift)
import Control.Monad.IO.Class (liftIO)
import Data.List (delete)
import GHC.Prim (RealWorld)
import System.Random.MWC

-- | A custom application of the ReaderT monad, allowing threaded access to a
-- random generator, supplied when the generation function is called at top
-- level (i.e. via runPadsGen)
newtype PadsGen a = PadsGen { unPadsGen :: ReaderT GenIO IO a }
  -- Here 'Gen RealWorld' is equal to 'GenIO' above. We can't write
  -- 'GenIO' here because 'GenIO' is defined as 'Gen (PrimState IO)',
  -- 'PrimState' is a type family, and type family instances aren't
  -- allowed. It's probably a bug, but GHC will allow us to derive a
  -- @MonadReader GenIO@ instance here, but we then get type errors at
  -- use sites of 'MonadReader' functions, e.g. 'ask'.
  deriving (Functor, Applicative, Monad, MonadReader (Gen RealWorld), MonadIO)

-- | Provides the requisite random number generator to the supplied generation
-- computation and returns the result as a value in IO.
runPadsGen :: PadsGen a -> IO a
runPadsGen genM = do
  gen <- createSystemRandom
  runReaderT (unPadsGen genM) gen

-- | The types 'randNum'/'randNumBound'/'randNumBetween' return are dictated by
-- the types of their callers.
randNum :: (Variate a) => PadsGen a
randNum = ask >>= (liftIO . uniform)

-- | A number with bounds i and j
randNumBetween :: (Integral a, Variate a) => a -> a -> PadsGen a
randNumBetween i j = ask >>= (liftIO . (uniformR (i, j)))

-- | A number with an upper bound i
randNumBound :: (Integral a, Variate a) => a -> PadsGen a
randNumBound i = randNumBetween 0 i

-- | As they aren't members of the System.Random.MWC class Variate, Integers
-- require special treatment - convert them from doubles.
randInteger :: PadsGen Integer
randInteger = randIntegerBound (2^1023)

-- | See randInteger
randIntegerBound :: Integral a => a -> PadsGen Integer
randIntegerBound i = do
  gen <- ask
  i' <- liftIO $ uniformR (0 :: Double, (fromIntegral i) :: Double) gen
  (return . toInteger . floor) i'

-- | Choose an element from a list at random
randElem :: [a] -> PadsGen a
randElem xs = do
  gen <- ask
  r <- liftIO $ fromIntegral <$> uniformR (0, length xs - 1) gen
  return $ xs !! r

-- | A random letter from English upper and lowercase letters
randLetter :: PadsGen Char
randLetter = randElem letters

-- | A random letter from English upper and lowercase letters, excluding the
-- provided character
randLetterExcluding :: Char -> PadsGen Char
randLetterExcluding c = randElem (delete c letters)

letters :: [Char]
letters = ['A'..'Z'] ++ ['a'..'z']

listLengthLimit = 100

-- | A list of random length, provided a PadsGen generator. Optionally also
-- paramaterized by an Int, which if provided will be the length of the list
randList :: PadsGen a -> Maybe Int -> PadsGen [a]
randList padsGen intM = do
  i <- case intM of
    Just x  -> return x
    Nothing -> ask >>= (liftIO . (uniformR (1, listLengthLimit)))
  replicateM i padsGen

recLimit = 10000

-- | Provided a predicate, function that transforms a seed value, recursion
-- limit, and seed value, generates a value of the seed's type that satisfies
-- the predicate
untilM :: Monad m => (a -> Bool) -> (a -> m a) -> Integer -> a -> m a
untilM p f i z = do
  when (i <= 0)
    (error $ "untilM: recursion too deep. Your description probably "
          ++ "contains a too-narrow constraint to efficiently "
          ++ "generate data that satisfy it. To increase "
          ++ "the recursion limit ('recLimit' in Generation.hs), "
          ++ "currently set to " ++ show recLimit
          ++ ", edit it and try again.")
  if p z
    then return z
    else f z >>= untilM p f (i - 1)

-- | A random instance of the provided PadsGen that satisfies the provided
-- constraint
randWithConstraint :: PadsGen a -> (a -> Bool) -> PadsGen a
randWithConstraint padsGen pred = do
  x <- padsGen
  x' <- untilM pred (const padsGen) recLimit x
  return x'
