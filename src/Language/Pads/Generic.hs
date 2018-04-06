{-# LANGUAGE TypeFamilies, ConstraintKinds, MultiParamTypeClasses
           , FunctionalDependencies, ScopedTypeVariables, FlexibleContexts
           , Rank2Types, FlexibleInstances #-}
{-# OPTIONS_HADDOCK prune #-}
{-|
  Module      : Language.Pads.Generic
  Description : Default parse values using GHC Generics
  Copyright   : (c) 2011
                Kathleen Fisher <kathleen.fisher@gmail.com>
                John Launchbury <john.launchbury@gmail.com>
  License     : MIT
  Maintainer  : Karl Cronburg <karl@cs.tufts.edu>
  Stability   : experimental

-}
module Language.Pads.Generic where

import Language.Pads.MetaData
import Language.Pads.PadsParser
import qualified Language.Pads.Errors as E
import qualified Language.Pads.Source as S
import Language.Pads.PadsPrinter
import qualified Data.ByteString as B
import qualified Control.Exception as CE
import Data.Data
import Data.Generics.Aliases (extB, ext1B)
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Set (Set(..))
import qualified Data.Set as Set
import Language.Pads.Errors

import System.Posix.Types
import Foreign.C.Types
import System.CPUTime

type Pads rep md = Pads1 () rep md

def :: Pads rep md => rep
def = def1 ()
defaultMd :: Pads rep md => rep -> md
defaultMd = defaultMd1 ()
parsePP :: Pads rep md => PadsParser (rep,md)
parsePP = parsePP1 ()
printFL :: Pads rep md => PadsPrinter (rep,md)
printFL = printFL1 ()
defaultRepMd :: Pads rep md => (rep,md)
defaultRepMd = defaultRepMd1 ()

parseRep :: Pads rep md => String -> rep
parseRep cs = fst $ fst $ parseStringInput parsePP cs

parseS   :: Pads rep md => String -> ((rep, md), String)
parseS cs = parseStringInput parsePP cs

parseBS   :: Pads rep md => B.ByteString -> ((rep, md), B.ByteString)
parseBS cs = parseByteStringInput parsePP cs

parseFile :: Pads rep md => FilePath -> IO (rep, md)
parseFile file = parseFileWith parsePP file

parseFileWithDisc :: Pads rep md => S.RecordDiscipline -> FilePath -> IO (rep, md)
parseFileWithDisc d file = parseFileWithD d parsePP file

printS :: Pads rep md => (rep,md) -> (String)
printS = S.byteStringToStr . printBS

printRep :: Pads rep md => rep -> String
printRep = printRep1 ()

printBS :: Pads rep md => (rep,md) -> (B.ByteString)
printBS r = let f = (printFL r) in f B.empty

printFile :: Pads rep md => FilePath -> (rep,md) -> IO ()
printFile filepath r = do
  let str = printBS r
  B.writeFile filepath str

printFileRep :: Pads rep md => FilePath -> rep -> IO ()
printFileRep filepath r = printFile filepath (r,defaultMd r)

type family PadsArg rep :: *

class (Data rep, PadsMD md, PadsMD (Meta rep)) => Pads1 arg rep md | rep -> md, rep -> arg where
  def1 :: arg -> rep
  def1 =  \_ -> gdef
  defaultMd1 :: arg -> rep -> md
  defaultMd1 _ _ = myempty
  parsePP1  :: arg -> PadsParser (rep,md)
  printFL1 :: arg -> PadsPrinter (rep,md)
  defaultRepMd1 :: arg -> (rep,md)
  defaultRepMd1 arg = (rep,md) where
    rep = def1 arg
    md = defaultMd1 arg rep

parseRep1 :: Pads1 arg rep md => arg -> String -> rep
parseRep1 arg cs = fst $ fst $ parseStringInput (parsePP1 arg) cs

parseS1 :: Pads1 arg rep md => arg -> String -> ((rep, md), String)
parseS1 arg cs = parseStringInput (parsePP1 arg) cs

parseBS1 :: Pads1 arg rep md => arg -> B.ByteString -> ((rep, md), B.ByteString)
parseBS1 arg cs = parseByteStringInput (parsePP1 arg) cs


parseString1 :: Pads1 arg rep md => arg-> String -> (rep, md)
parseString1 arg str = parseStringWith (parsePP1 arg) str

parseFile1 :: Pads1 arg rep md => arg-> FilePath -> IO (rep, md)
parseFile1 arg file = parseFileWith (parsePP1 arg) file

parseFile1WithDisc :: Pads1 arg rep md => S.RecordDiscipline -> arg -> FilePath -> IO (rep, md)
parseFile1WithDisc d arg file = parseFileWithD d (parsePP1 arg) file

printS1 :: Pads1 arg rep md => arg -> (rep,md) -> (String)
printS1 arg (rep,md) = S.byteStringToStr (printBS1 arg (rep,md))

printRep1 :: Pads1 arg rep md => arg -> rep -> String
printRep1 arg rep = printS1 arg (rep,defaultMd1 arg rep)

printBS1 :: Pads1 arg rep md => arg -> (rep,md) -> (B.ByteString)
printBS1 arg r = let f = (printFL1 arg r) in f B.empty
printFile1 :: Pads1 arg rep md => arg -> FilePath -> (rep,md) -> IO ()
printFile1 arg filepath r = do
  let str = printBS1 arg r
  B.writeFile filepath str

printFileRep1 :: Pads1 arg rep md => arg -> FilePath -> rep -> IO ()
printFileRep1 arg filepath r = printFile1 arg filepath (r,defaultMd1 arg r)

parseStringWith  :: (Data rep, PadsMD md) => PadsParser (rep,md) -> String -> (rep,md)
parseStringWith p str = fst $ parseStringInput p str

parseFileWith  :: (Data rep, PadsMD md) => PadsParser (rep,md) -> FilePath -> IO (rep,md)
parseFileWith p file = do
   result <- CE.try (parseFileInput p file)
   case result of
     Left (e::CE.SomeException) -> return (gdef, replace_md_header gdef
                                                 (mkErrBasePD (E.FileError (show e) file) Nothing))
     Right r -> return r

parseFileWithD  :: (Data rep, PadsMD md) => S.RecordDiscipline -> PadsParser (rep,md) -> FilePath -> IO (rep,md)
parseFileWithD d p file = do
   result <- CE.try (parseFileInputWithDisc d p file)
   case result of
     Left (e::CE.SomeException) -> return (gdef, replace_md_header gdef
                                                 (mkErrBasePD (E.FileError (show e) file) Nothing))
     Right r -> return r

{- Generic function for computing the default for any type supporting Data a interface -}
getConstr :: DataType -> Constr
getConstr ty =
   case dataTypeRep ty of
        AlgRep cons -> head cons
        IntRep      -> mkIntegralConstr ty 0
        FloatRep    -> mkRealConstr ty 0.0
        CharRep     -> mkCharConstr ty '\NUL'
        NoRep       -> error "PADSC: Unexpected NoRep in PADS type"

gdef :: Data a => a
gdef = def_help
  where
    def_help
     =   let ty = dataTypeOf (def_help)
             constr = getConstr ty
         in fromConstrB gdef constr

ext2 :: (Data a, Typeable t)
     => c a
     -> (forall d1 d2. (Data d1, Data d2) => c (t d1 d2))
     -> c a
ext2 def ext = maybe def id (dataCast2 ext)

newtype B x = B {unB :: x}

ext2B :: (Data a, Typeable t)
      => a
      -> (forall b1 b2. (Data b1, Data b2) => t b1 b2)
      -> a
ext2B def ext = unB ((B def) `ext2` (B ext))



class BuildContainer2 c key item where
  buildContainer2 :: [(key,item)] -> c key item
  toList2         :: c key item -> [(key,item)]

instance Ord key => BuildContainer2 Map key a  where
  buildContainer2 = Map.fromList
  toList2         = Map.toList

class BuildContainer1 c key item where
  buildContainer1 :: [(key,item)] -> c (key, item)
  toList1         :: c (key, item) ->  [(key,item)]

instance (Ord a,Ord key) => BuildContainer1 Set key a  where
  buildContainer1 = Set.fromList
  toList1         = Set.toList

instance BuildContainer1 [] key a  where
  buildContainer1 = id
  toList1         = id

