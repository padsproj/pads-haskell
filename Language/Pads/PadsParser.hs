{-# LANGUAGE NamedFieldPuns #-}
{-|
  Module      : Language.Pads.PadsParser
  Description : The parsing monad for Pads
  Copyright   : (c) 2011
                Kathleen Fisher <kathleen.fisher@gmail.com>
                John Launchbury <john.launchbury@gmail.com>
  License     : BSD3
  Maintainer  : Karl Cronburg <karl@cs.tufts.edu>
  Stability   : experimental

  These are the combinators used to build PADS parsers. In this module we define
  the 'PadsParser' parsing monad which operates in a model where each parsing
  step (bind in the monad) runs a function `f :: Source -> (a, Source)`. This is
  similar to how the 'Read' typeclass implements parsing where we return the thing
  parsed by `f` of type `a` along with the remainder source input of type
  `Source`.

  Some important notes for future developers:
  - 'primPads' and 'queryP' below let you define new PadsParsers using Haskell
    functions without having to crack open the monad yourself.
  - As soon as the monad encounters a failure, we stop parsing and return the
    parsed result as far as we got in the input string.

-}

module Language.Pads.PadsParser where

import qualified Language.Pads.Source as S
import Language.Pads.Errors
import Language.Pads.MetaData
import Language.Pads.RegExp
import Data.Char
import Data.Word

import Control.Applicative (Applicative(..))
import Control.Monad

-- | Take a 'PadsParser' for some type and an input 'String' and parse the
-- 'String' using the 'PadsParser'.
parseStringInput :: PadsParser a -> String -> (a,String)
parseStringInput pp cs =
  let ((r,rest),b) = pp # S.padsSourceFromString cs
  in  (r, S.padsSourceToString rest)

-- | Same as 'parseStringInput' but with a 'RawStream' source as input.
parseByteStringInput :: PadsParser a -> S.RawStream -> (a, S.RawStream)
parseByteStringInput pp cs =
  let ((r,rest),b) = pp # S.padsSourceFromByteString cs
  in  (r, S.padsSourceToByteString rest)

-- | Same as 'parseStringInput' but with a 'FilePath' as input and 'IO' output.
parseFileInput :: PadsParser a -> FilePath -> IO a
parseFileInput pp file = do
  source <- S.padsSourceFromFile file
  let ((r,rest),b) = pp # source
  return r

-- | Same as 'parseFileInput' but with the ability to specify a non-default
-- record discipline.
parseFileInputWithDisc :: S.RecordDiscipline -> PadsParser a -> FilePath -> IO a
parseFileInputWithDisc d pp file =  do
  source <- S.padsSourceFromFileWithDisc d file
  let ((r,rest),b) = pp # source
  return r

-------------------------------------------------------------------------------
-- * The Pads Parsing Monad

-- | A Pads parser is a function over inputs to some type `a` and the remaining
-- input.
newtype PadsParser a = PadsParser { (#) :: S.Source -> Result (a,S.Source) }

-- | A Pads parse result is just a tuple of the type parsed and a boolean
-- indicating whether or not a parse error occured. If the boolean is False,
-- the result type has been populated with default values. See
-- "Language.Pads.Generic" for the type class implementing default values, and
-- "Language.Pads.CoreBaseTypes" for definitions of default values for the
-- built-in types.
type    Result a = (a,Bool)

-- | A Pads parser can be mapped over, which just says we need to run the
-- parser, grab the resulting parsed value, and apply the function we're mapping
-- to that result.
instance Functor PadsParser where
  fmap f p = PadsParser $ \bs -> let ((x,bs'),b) = p # bs in
                                   ((f x, bs'),b)

-- | This monad instance for Pads parsers looks just like any other sequencing
-- monad (run the first one and pipe the result into the second) with one thing
-- added: if any results on the way are bad, then the whole parse is bad.
instance Monad PadsParser where
  return r = PadsParser $ \bs -> ((r,bs), True)
  p >>= f  = PadsParser $ \bs -> let ((v,bs'),b)   = p # bs
                                     ((w,bs''),b') = f v # bs'
                                 in ((w,bs''), b && b')

-- | Applicative instance for 'PadsParser' to satisfy GHC
instance Applicative PadsParser where
  pure  = return
  (<*>) = ap

-- | A pads parsing combinator used by other pads parsers when they detect a
-- parse error.
badReturn r = PadsParser $ \bs -> ((r,bs), False)

-- | 
mdReturn (rep,md) = PadsParser $
    \bs -> (((rep,md),bs), numErrors (get_md_header md) == 0)

returnClean :: t -> PadsParser (t, Base_md)
returnClean x = return (x, cleanBasePD)

returnError :: t -> ErrMsg -> PadsParser (t, Base_md)
returnError x err = do loc <- getLoc
                       badReturn (x, mkErrBasePDfromLoc err loc)

infixl 5 =@=, =@

p =@= q = do {(f,g) <- p; (rep,md) <- q; return (f rep, g md) }
p =@  q = do {(f,g) <- p; (rep,md) <- q; return (f, g md) }

-------------------------------------------------------------------------------
-- * Source manipulation functions

-- | Run a pure function on the current source input from inside the
-- 'PadsParser' monad. Used for detecting things like 'isEOF', 'isEOR', or for
-- peaking at the current head of the input with 'peekHeadP'.
queryP :: (S.Source -> a) -> PadsParser a
queryP f = PadsParser $ \bs -> ((f bs,bs), True)

-- | Run a pure function to mutate the current input source.
primPads :: (S.Source -> (a,S.Source)) -> PadsParser a
primPads f = PadsParser $ \bs -> (f bs, True)

-- | Lift a function which runs in the 'Maybe' monad to run in the 'PadsParser'
-- monad with the same semantics as 'primPads', with the added ability that a
-- Nothing produces a parse failure.
liftStoP :: (S.Source -> Maybe (a,S.Source)) -> a -> PadsParser a
liftStoP f def = PadsParser $ \bs ->
                 case f bs of
                   Nothing      -> ((def,bs), False)
                   Just (v,bs') -> ((v,bs'), True)

-- | Replace the source in the given 'Result' with the given 'Source'
replaceSource :: S.Source -> Result (a,S.Source) -> Result (a,S.Source)
replaceSource bs ((v,_),b) = ((v,bs),b)

-------------------------------------------------------------------------------
-- * Monad choice combinators

-- The monad is non-backtracking. The only choice point is at ChoiceP

choiceP :: [PadsParser a] -> PadsParser a
choiceP ps = foldr1 (<||>) ps

(<||>) :: PadsParser a -> PadsParser a -> PadsParser a
p <||> q = PadsParser $ \bs -> (p # bs) <++> (q # bs)

(<++>) :: Result a -> Result a -> Result a
(r, True)    <++> _  = (r, True)
(r1, False)  <++> r2 = r2 -- A number of functions rely on this being r2

-------------------------------------------------------------------------------

parseTry :: PadsParser a -> PadsParser a
parseTry p = PadsParser $ \bs -> replaceSource bs (p # bs)

-------------------------------------------------------------------------------
-- * Parsers for Pads language features

-- | 
parseConstraint :: PadsMD md =>
    PadsParser(rep,md) -> (rep -> md -> Bool) -> PadsParser(rep, md)
parseConstraint p pred = do
  (rep,md) <- p
  mdReturn (rep, replace_md_header md (constraintReport (pred rep md) md))

constraintReport isGood md = Base_md {numErrors = totErrors, errInfo = errors}
  where
    Base_md {numErrors, errInfo} = get_md_header md
    totErrors = if isGood then numErrors else numErrors + 1
    errors = if totErrors == 0 then Nothing else
       Just(ErrInfo {msg = if isGood then UnderlyingTypedefFail
                                     else PredicateFailure,
                     position = join $ fmap position errInfo})

-------------------------------------------------------------------------------

parseTransform :: PadsMD dmd =>
    PadsParser (sr,smd) -> (S.Pos->(sr,smd)->(dr,dmd)) -> PadsParser (dr,dmd)
parseTransform sParser transform = do
  { begin_loc <- getLoc
  ; src_result <- sParser
  ; end_loc <- getLoc
  ; let src_pos = S.locsToPos begin_loc end_loc
  ; return (transform src_pos src_result)
  }

-------------------------------------------------------------------------------

parsePartition :: PadsMD md =>
    PadsParser(rep,md) -> S.RecordDiscipline -> PadsParser(rep, md)
parsePartition p newDisc = do
  { oldDisc <- queryP S.getRecordDiscipline
  ; primPads (S.setRecordDiscipline newDisc)
  ; x <- p
  ; primPads (S.setRecordDiscipline oldDisc)
  ; return x
  }

-------------------------------------------------------------------------------

parseListNoSepNoTerm :: PadsMD md =>
    PadsParser (rep,md) -> PadsParser ([rep], (Base_md, [md]))
parseListNoSepNoTerm p = listReport (parseMany p)

parseListSepNoTerm :: (PadsMD md, PadsMD mdSep) =>
    PadsParser (repSep,mdSep) -> PadsParser(rep, md) -> PadsParser ([rep], (Base_md, [md]))
parseListSepNoTerm sep p = listReport (parseManySep sep p)

parseListNoSepLength :: (PadsMD md) =>
    Int -> PadsParser(rep, md) -> PadsParser ([rep], (Base_md, [md]))
parseListNoSepLength i p = listReport (parseCount i p)

parseListSepLength :: (PadsMD md, PadsMD mdSep) =>
    PadsParser (repSep,mdSep) -> Int -> PadsParser(rep, md) -> PadsParser ([rep], (Base_md, [md]))
parseListSepLength sep n p = listReport (parseCountSep n sep p)

parseListNoSepTerm :: (PadsMD md, PadsMD mdTerm) =>
    PadsParser (repTerm,mdTerm) -> PadsParser(rep, md) -> PadsParser ([rep], (Base_md, [md]))
parseListNoSepTerm term p = listReport (parseManyTerm term p)

parseListSepTerm :: (PadsMD md, PadsMD mdSep, PadsMD mdTerm) =>
    PadsParser (repSep,mdSep) -> PadsParser (repTerm,mdTerm) ->
    PadsParser(rep, md) -> PadsParser ([rep], (Base_md, [md]))
parseListSepTerm sep term p = listReport (parseManySepTerm sep term p)


listReport  :: PadsMD b => PadsParser [(a, b)] -> PadsParser ([a], (Base_md, [b]))
listReport p = do
  listElems <- p
  let (reps, mds) = unzip listElems
  let hmds = map get_md_header mds
  return (reps, (mergeBaseMDs hmds, mds))


-----------------------------------

parseMany :: PadsMD md => PadsParser (rep,md) -> PadsParser [(rep,md)]
parseMany p = do (r,m) <- p
                 if (numErrors (get_md_header m) == 0)
                   then do { rms <- parseMany p
                           ; return ((r,m) : rms)}
                   else badReturn []

              <||> return []


parseManySep :: (PadsMD md, PadsMD mdSep) => PadsParser (repSep,mdSep) -> PadsParser(rep, md) -> PadsParser [(rep,md)]
parseManySep sep p = do { rm <- p
                        ; rms <- parseManySep1 sep p
                        ; return (rm : rms)
                        }

parseManySep1 :: (PadsMD md, PadsMD mdSep) => PadsParser (repSep,mdSep) -> PadsParser(rep, md) -> PadsParser [(rep,md)]
parseManySep1 sep p = do (r,m) <- sep
                         if (numErrors (get_md_header m) == 0)
                           then parseManySep sep p
                           else badReturn []
                      <||> return []

-----------------------------------


parseCount :: (PadsMD md) => Int -> PadsParser(rep, md) -> PadsParser [(rep,md)]
parseCount n p = sequence (replicate n p)


parseCountSep :: (PadsMD md) =>
    Int -> PadsParser rmdSep -> PadsParser(rep, md) -> PadsParser [(rep,md)]
parseCountSep n sep p  | n <= 0 = return []
parseCountSep n sep p = do
   rm <- p
   rms <- sequence $ replicate (n-1) (sep >> p)
   return (rm:rms)



-----------------------------------



parseManyTerm :: (PadsMD md, PadsMD mdTerm) =>
    PadsParser (repTerm,mdTerm) -> PadsParser(rep, md) -> PadsParser [(rep,md)]
parseManyTerm term p = (term >> return [])
                  <||> (ifEOFP >> return [])
                  <||> do { rm <- p
                          ; rms <- parseManyTerm term p
                          ; return (rm:rms) }



parseManySepTerm :: (PadsMD md, PadsMD mdSep, PadsMD mdTerm) =>
    PadsParser (repSep,mdSep) -> PadsParser (repTerm,mdTerm) -> PadsParser(rep, md) -> PadsParser [(rep,md)]
parseManySepTerm sep term p = (term >> return [])
                         <||> (ifEOFP >> return [])
                         <||> scan
  where
  scan = do (rep, md) <- p
            (terminated,junk) <- seekSep sep term
            case junk of
              [] -> if terminated then return [(rep,md)] else
                    do rms <- scan
                       return ((rep,md):rms)
              _  -> do sepLoc <- getLoc
                       let report = junkReport md sepLoc junk
                       if terminated then
                         badReturn [(rep,report)]
                         else do
                           rms <- scan
                           badReturn ((rep,report) : rms)


seekSep sep term = (term >> return (True, []))
              <||> (ifEOFP >> return (True, []))
              <||> (sep >> return (False, []))
              <||> do { b <- isEORP
                      ; if b then badReturn (False, []) else
                        do { c <- takeHeadP
                           ; (b,cs) <- seekSep sep term
                           ; badReturn (b, c:cs)
                           }
                         }

junkReport md loc junk = replace_md_header md mergeMD
  where
    mdSep   = mkErrBasePDfromLoc (ExtraStuffBeforeTy junk "seperator" ) loc
    mergeMD = mergeBaseMDs [get_md_header md, mdSep]



-------------------------------------------------



getLoc :: PadsParser S.Loc
getLoc = queryP S.getSrcLoc

isEOFP, isEORP :: PadsParser Bool
isEOFP = queryP S.isEOF
isEORP = queryP S.isEOR

ifEOFP, ifEORP :: PadsParser ()
ifEOFP = do { b <- isEOFP; if b then return () else badReturn ()}
ifEORP = do { b <- isEORP; if b then return () else badReturn ()}


takeP :: Integral a => a -> PadsParser String
takeP n = primPads (S.take (fromInt n))

takeBytesP :: Integral a => a -> PadsParser S.RawStream
takeBytesP n = primPads (S.takeBytes (fromInt n))

takeBytesNBP :: Integral a => a -> PadsParser S.RawStream
takeBytesNBP n = primPads (S.takeBytesNB (fromInt n))

takeBitsP :: Integral a => a -> PadsParser Integer
takeBitsP b = primPads (S.takeBits (fromInt b))

takeBits8P :: Integral a => a -> PadsParser Word8
takeBits8P b = primPads (S.takeBits8 (fromInt b))

takeBits16P :: Integral a => a -> PadsParser Word16
takeBits16P b = primPads (S.takeBits16 (fromInt b))

takeBits32P :: Integral a => a -> PadsParser Word32
takeBits32P b = primPads (S.takeBits32 (fromInt b))

takeBits64P :: Integral a => a -> PadsParser Word64
takeBits64P b = primPads (S.takeBits64 (fromInt b))

fromInt :: (Integral a1, Num a) => a1 -> a
fromInt n = fromInteger $ toInteger n



-------------------------------------------------



peekHeadP :: PadsParser Char
peekHeadP = queryP S.head

takeHeadP :: PadsParser Char
takeHeadP = primPads S.takeHead

takeHeadStrP :: String -> PadsParser Bool
takeHeadStrP str = primPads (S.takeHeadStr str)

-- Return string is junk before found string
scanStrP :: String -> PadsParser (Maybe String)
scanStrP str = primPads (S.scanStr str)


regexMatchP ::RE -> PadsParser (Maybe String)
regexMatchP re = primPads (S.regexMatch re)

regexStopP :: RE -> PadsParser (Maybe String)
regexStopP re = primPads (S.regexStop re)

scanP :: Char -> PadsParser Bool
scanP c = primPads (\s -> let (f,r,e) = S.scanTo c s in (f,r))

getAllP :: PadsParser String
getAllP = primPads S.drainSource

getAllBinP :: PadsParser S.RawStream
getAllBinP = primPads S.rawSource

drainSourceNBP :: PadsParser String
drainSourceNBP = primPads S.drainSourceNB

satisfy p = primPads loop
 where loop s = if S.isEOF s || S.isEOR s then ([],s) else
          let c = S.head s in
          if p c then
            let (xs,s') = loop (S.tail s) in
            (c:xs, s')
          else
            ([],s)

satisfyNBP :: (Char -> Bool) -> (PadsParser String)
satisfyNBP p = primPads (S.satisfyNB p)

digitListToInt :: Bool -> [Char] -> Int
digitListToInt isNeg digits = if isNeg then negate raw else raw
  where
    raw = foldl (\a d ->10*a + digitToInt d) 0 digits

-------------------------


doLineBegin :: PadsParser ((), Base_md)
doLineBegin = do
  rbegErr <- primPads S.srcLineBegin
  case rbegErr of
    Nothing -> returnClean ()
    Just err -> returnError () (LineError err)


doLineEnd :: PadsParser ((), Base_md)
doLineEnd = do
  rendErr <- primPads S.srcLineEnd
  case rendErr of
    Nothing -> returnClean ()
    Just err -> returnError () (LineError err)
