{-# LANGUAGE NamedFieldPuns, RecordWildCards, DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK prune #-}
{-|
  Module      : Language.Pads.Source
  Description : Host language representation of data input
  Copyright   : (c) 2011
                Kathleen Fisher <kathleen.fisher@gmail.com>
                John Launchbury <john.launchbury@gmail.com>
  License     : MIT
  Maintainer  : Karl Cronburg <karl@cs.tufts.edu>
  Stability   : experimental
-}

module Language.Pads.Source where

import qualified Data.ByteString as B   -- abstraction for input data
import qualified Text.Regex.Posix as TRP
import Language.Pads.RegExp                        -- user-specified regular expressions
import Text.PrettyPrint.Mainland as PP
import Text.PrettyPrint.Mainland.Class

import Data.Int
import Data.Data
import Data.Word
import Data.Char
import Data.Bits (shiftR, shiftL, (.&.))
import qualified Data.ByteString.Char8 as Char8

type RawStream = B.ByteString   -- This is the type that should be used in other files!!!

{-| Input source abstraction -}
data Source = Source
  { current  :: B.ByteString      -- ^ The current input before EOR
  , rest     :: B.ByteString      -- ^ The rest of the input after the next EOR
  , loc      :: Loc               -- ^ The current location 'Loc' in the input
  , bit      :: Int               -- ^ Bit offset into the current input being parsed
  , disc     :: RecordDiscipline  -- ^ The record discipline of this input source
  , eorAtEOF :: Bool  -- ^ Relevant for seperator-based record disciplines: Single, Multi
                      -- Set when current record is last record and separtor appeared at end.
  }

-- | A record discipline specifies the manner by which pads should partition the
-- input into records. Note that the record character gets consumed internally
-- by the parsing monad.
data RecordDiscipline =
    Single Word8          -- ^ Split input based on a single 8-bit unsigned integer (character)
  | Multi B.ByteString    -- ^ Split input based on more than one character
  | Bytes Int             -- ^ Split the input into records every 'Int' characters
  | NoPartition           -- ^ No partitioning of the input - all input data is in the 'current' field
  | NoDiscipline          -- ^ No discipline is currently installed; all input data is in 'rest' field

-- | Record discipline for Unix newlines
newline = Single (chrToWord8 '\n')
-- | Record discipline for Windows CRLF newlines
windows = Multi  (B.pack (strToWord8s "\r\n"))
-- | Record discipline for every n characters
bytes n = Bytes n
-- | No record discipline
none    = NoPartition

{-| Source location information. -}
data Loc = Loc
  { recordNumber  :: Int64 -- ^ Number of records parsed so far (i.e. record index)
  , byteOffset    :: Int64 -- ^ Total number of bytes parsed into the source input so far
  } deriving (Typeable, Data,Eq, Ord, Show)

-- | A span in the source input, covering a contiguous range of the 'Source'
-- input. AFAIK there's no distinction between the states where @begin == end@
-- and where @end == Nothing@.
data Span = Span
  { begin      :: Loc       -- ^ Start location of the 'Span'
  , end        :: Maybe Loc -- ^ End location of the 'Span', inclusive
  } deriving (Typeable, Data, Eq, Ord, Show)

-- | Initial instance of a 'Loc'
zeroLoc   = Loc {recordNumber = 0,  byteOffset = 0}

-- | A span starting at the beginning of the file and containing nothing.
zeroSpan = locToSpan zeroLoc

-- | Parse the most significant bit in a byte first
zeroBit = 7

-- | Increment how many records have been seen in the given 'Loc'
incRecordNumber :: Loc -> Loc
incRecordNumber Loc{recordNumber, ..} = Loc{ recordNumber = recordNumber+1
                                       , byteOffset = 0}

-- | Decrement how many records have been seen in the given 'Loc'
decLineNumber :: Loc -> Loc
decLineNumber Loc{recordNumber, ..} = Loc{recordNumber=recordNumber-1, byteOffset=0}

-- | Increment the offset of the 'Loc' by one
incOffset :: Loc -> Loc
incOffset l@Loc{byteOffset} = l { byteOffset = byteOffset + 1 }

-- | Increment the offset of the given 'Loc' by some number
incOffsetBy :: Loc -> Int -> Loc
incOffsetBy l@Loc{byteOffset} n = l { byteOffset = byteOffset + fromIntegral n }

-- | Decrement the offset of the given 'Loc' by one
decOffset :: Loc -> Loc
decOffset l@Loc{byteOffset} = l { byteOffset = byteOffset - 1 }

getSrcLoc :: Source -> Loc
getSrcLoc = loc

getRecordDiscipline :: Source -> RecordDiscipline
getRecordDiscipline = disc


-------------------------------------------------------------------------------
-- * Source Creation

-- | An empty Source with reasonable defaults for everything.
emptySource = Source
  { current   = B.empty
  , rest      = B.empty
  , loc       = zeroLoc
  , bit       = zeroBit
  , eorAtEOF  = False
  , disc      = newline
  }

-- | Stuff the given 'String' into a 'Source' with a newline discipline by
-- default (see 'padsSourceFromByteString')
padsSourceFromString :: String -> Source
padsSourceFromString str = padsSourceFromByteString (strToByteString str)

-- | Stuff the given 'String' into a 'Source' with the given record discipline
padsSourceFromStringWithDisc :: RecordDiscipline -> String -> Source
padsSourceFromStringWithDisc d str = padsSourceFromByteStringWithDisc d (strToByteString str)

-- | Read a 'Source' from disk
padsSourceFromFile :: FilePath -> IO Source
padsSourceFromFile file = do
  bs <- B.readFile file
  return (padsSourceFromByteString bs)

-- | Read a 'Source' from disk using the given record discipline
padsSourceFromFileWithDisc :: RecordDiscipline -> FilePath -> IO Source
padsSourceFromFileWithDisc d file = do
  bs <- B.readFile file
  return (padsSourceFromByteStringWithDisc d bs)

-- | Construct a 'Source' from the given 'ByteString', preparing the first
-- record immediately.
padsSourceFromByteString :: B.ByteString -> Source
padsSourceFromByteString bs =
    let rawSource = Source{ current  = B.empty
                          , rest     = bs
                          , loc      = zeroLoc
                          , bit      = zeroBit
                          , disc     = newline
                          , eorAtEOF = False
                          }
    in getNextRecord rawSource

-- | Same as 'padsSourceFromByteString' but with a record discipline
padsSourceFromByteStringWithDisc :: RecordDiscipline -> B.ByteString -> Source
padsSourceFromByteStringWithDisc d bs =
    let rawSource = Source{ current  = B.empty
                          , rest     = bs
                          , loc      = zeroLoc
                          , bit      = zeroBit
                          , disc     = d
                          , eorAtEOF = False
                          }
    in getNextRecord rawSource

-- | Whether or not the 'Source' has consumed all available input
isEOF :: Source -> Bool
isEOF (s @ Source{current, rest, eorAtEOF, ..}) = B.null current && B.null rest && not eorAtEOF

-- | Whether or not the 'Source' has consumed all input in the current record
isEOR :: Source -> Bool
isEOR = B.null . current

-------------------------------------------------------------------------------
-- * Record Manipulating Functions
{- Called when current is empty to get the next record, where the disc field defines what constitutes a record.
   NOOP when isEOF is already true. -}
getNextRecord :: Source -> Source
getNextRecord (s @ Source {current, rest, loc, bit, disc, eorAtEOF}) =
      if isEOF s then s
      else if eorAtEOF || B.null rest then
            (Source {current = B.empty, rest = B.empty, loc = incRecordNumber loc, bit = zeroBit, disc, eorAtEOF = False})
      else  (Source {current = nextLine, rest=residual, loc = incRecordNumber loc, bit = zeroBit, disc, eorAtEOF = eorAtEOF'}) --TODO: is this bit positioning sound?
        where (nextLine, residual, eorAtEOF') = breakUsingDisc rest disc

srcLineBegin :: Source -> (Maybe String, Source)
srcLineBegin s = (Nothing, s)

srcLineEnd :: Source -> (Maybe String, Source)
srcLineEnd s = if isEOF s
     then (Just "Found EOF when looking for EOR", s)
     else (Nothing, getNextRecord s)

{- External code should not set discipline to NoDiscipline; that is an internal state only to
   mark state between two legal external disciplines. -}
setRecordDiscipline :: RecordDiscipline -> Source -> ((),Source)
setRecordDiscipline r s =
  let s'   = unputCurrentLine s
      s'' = s'{disc = r}
  in ((),getNextRecord s'')

{- Merge current record back on to rest according to current record discipline.
   Resulting source will be in NoDiscipline state.
   Noop if called at EOF.
-}
unputCurrentLine :: Source -> Source
unputCurrentLine (s @ Source {current, rest, loc, disc, eorAtEOF}) =
      if isEOF s then s
      else case disc of
        Single n -> let rest' = if B.null rest
                                then if eorAtEOF
                                     then B.concat [current, B.singleton n]
                                     else current
                                else B.concat [current, B.singleton n, rest]
                        loc'  = if B.null current then loc else decLineNumber loc
                    in Source {current = B.empty, rest = rest', loc = loc', bit = zeroBit, disc = NoDiscipline, eorAtEOF = False}
        Multi  br -> let rest' = if B.null rest
                                then if eorAtEOF
                                     then B.concat [current, br]
                                     else current
                                else B.concat [current, rest]
                         loc'  = if B.null current then loc else decLineNumber loc
                    in Source {current = B.empty, rest = rest', loc = loc', bit = zeroBit, disc = NoDiscipline, eorAtEOF = False}
        Bytes n -> Source {current = B.empty, rest = B.append current rest, loc = decLineNumber loc, bit = zeroBit, disc = NoDiscipline, eorAtEOF = False}
        NoPartition -> Source {current = B.empty, rest = current, loc = decLineNumber loc, bit = zeroBit, disc = NoDiscipline, eorAtEOF = False}
        NoDiscipline -> s


breakUsingDisc :: B.ByteString -> RecordDiscipline -> (B.ByteString, B.ByteString, Bool)
breakUsingDisc bs rd = case rd of
  Single n -> let (nextLine, raw_residual) = B.break (\c->c == n) bs
                  residual = B.drop 1 raw_residual
                  eorAtEOF = (B.null residual) && (not $ B.null raw_residual)
              in  (nextLine, residual, eorAtEOF)
  Multi s  -> let (nextLine, raw_residual) = B.breakSubstring s bs
                  residual = B.drop (B.length s) raw_residual
                  eorAtEOF =  (B.null residual) && (not $ B.null raw_residual)
              in  (nextLine, residual, eorAtEOF)
  Bytes n ->  let (nextLine, residual) = B.splitAt n bs
              in  (nextLine, residual, False)
  NoPartition -> (bs, B.empty, False)
  NoDiscipline -> error "Pads Source: Attempt to partition source using internal discipline 'NoDiscipline'"

-------------------------------------------------------------------------------
-- * Converting Sources to Strings
--
padsSourceToString :: Source -> String
padsSourceToString = (map word8ToChr) . B.unpack . padsSourceToByteString

padsSourceToByteString :: Source -> B.ByteString
padsSourceToByteString = rest . unputCurrentLine

drainSource :: Source -> (String, Source)
drainSource s = (padsSourceToString s, emptySource)

drainSourceNB :: Source -> (String, Source)
drainSourceNB (s @ Source{current,loc, ..}) =
    let len = (B.length current) - (if bit == zeroBit then 0 else 1)
        (bs, s') = takeBits (len * 8) s
    in (map word8ToChr (numToWord8s bs []), emptySource)


{- XXX: Change call site to use no record discipline; then this call should just became an instance of drainSource -}
rawSource :: Source -> (B.ByteString, Source)
rawSource s = (padsSourceToByteString s, emptySource)

{- Return the rest of the current record as a string -}
restRec :: Source -> String
restRec = byteStringToStr . current

-------------------------------------------------------------------------------
-- * Operations within a single record
head :: Source -> Char
head = word8ToChr . headOrZero . current

headOrZero s = if B.null s then chrToWord8 '\0' else B.head s

peekHeadM :: Source -> (Maybe Char, Source)
peekHeadM (s @ Source{current,loc, ..}) =
  if B.null current then (Nothing, s) else (Just (Language.Pads.Source.head s), s)

takeHead :: Source -> (Char, Source)
takeHead (s @ Source{current,loc, ..}) =
    (word8ToChr $ B.head current, s{current = B.tail current, loc = incOffset loc})

partitionBS :: Integral a => B.ByteString -> a -> a -> (B.ByteString, B.ByteString, Bool)
partitionBS bS bitIndex bits =
    let part b bs = if bs > b + 1 then 1 + part zeroBit (bs - (b + 1)) else 1
        byteAlign = (bits - (bitIndex + 1)) `mod` 8 == 0
        withinByte = bits <= bitIndex + 1
        hd = B.take (part (fromIntegral bitIndex) (fromIntegral bits)) bS
        tl = B.drop (B.length hd - if not byteAlign then 1 else 0) bS
    in  (hd, tl, withinByte || not byteAlign)

accumulate :: Integral a => a -> (a, Int) -> (a, Int)
accumulate byte (num, pow) = ((byte * (256 ^ pow)) + num, pow + 1)

takeBits8 :: Integral a => a -> Source -> (Word8, Source)
takeBits8 b (s @ Source{current,loc,bit, ..}) =
    let (hd, tl, partial) = partitionBS current bit (fromIntegral b)
        bS    = map (\x -> fromIntegral x :: Word16) (B.unpack $ B.take 2 hd)
        bytes = fst $ foldr accumulate (0,0) bS
        mask  = (2 ^ b) - 1
        bits  = mask .&. shiftR bytes ((B.length hd * 8) - (fromIntegral b) - (zeroBit - bit))
    in  (fromIntegral bits, s{current = tl,
                 loc = incOffsetBy loc (B.length hd - if partial then 1 else 0),
                 bit = (zeroBit - (((zeroBit - bit) + (fromIntegral b)) `mod` 8))})

takeBits16 :: Integral a => a -> Source -> (Word16, Source)
takeBits16 b (s @ Source{current,loc,bit, ..}) =
    let (hd, tl, partial) = partitionBS current bit (fromIntegral b)
        bS    = map (\x -> fromIntegral x :: Word32) (B.unpack $ B.take 3 hd)
        bytes = fst $ foldr accumulate (0,0) bS
        mask  = (2 ^ b) - 1
        bits  = mask .&. shiftR bytes ((B.length hd * 8) - (fromIntegral b) - (zeroBit - bit))
    in  (fromIntegral bits, s{current = tl,
                 loc = incOffsetBy loc (B.length hd - if partial then 1 else 0),
                 bit = (zeroBit - (((zeroBit - bit) + (fromIntegral b)) `mod` 8))})

takeBits32 :: Integral a => a -> Source -> (Word32, Source)
takeBits32 b (s @ Source{current,loc,bit, ..}) =
    let (hd, tl, partial) = partitionBS current bit (fromIntegral b)
        bS    = map (\x -> fromIntegral x :: Word64) (B.unpack $ B.take 5 hd)
        bytes = fst $ foldr accumulate (0,0) bS
        mask  = (2 ^ b) - 1
        bits  = mask .&. shiftR bytes ((B.length hd * 8) - (fromIntegral b) - (zeroBit - bit))
    in  (fromIntegral bits, s{current = tl,
                 loc = incOffsetBy loc (B.length hd - if partial then 1 else 0),
                 bit = zeroBit - (((zeroBit - bit) + (fromIntegral b)) `mod` 8)})

takeBits64 :: Integral a => a -> Source -> (Word64, Source)
takeBits64 b s = let (bits, s') = takeBits b s
                 in (fromIntegral bits, s')

tobinary :: Integer -> Integer
tobinary x
    | (div x 2) == 0 = x
    | otherwise = (mod x 2) + (10 * (tobinary $ div x 2))

takeBits :: Integral a => a -> Source -> (Integer, Source)
takeBits b (s @ Source{current,loc,bit, ..}) =
    let (hd, tl, partial) = partitionBS current bit (fromIntegral b)
        bS    = map fromIntegral (B.unpack hd)
        bytes = fst $ foldr accumulate (0,0) bS
        mask  = (2 ^ b) - 1
        shiftAmt = max 0 ((B.length hd * 8) - (fromIntegral b) - (zeroBit - bit))
        bits  = mask .&. shiftR bytes shiftAmt
    in  (bits, s{current = tl,
                 loc = incOffsetBy loc (B.length hd - if partial then 1 else 0),
                 bit = zeroBit - (((zeroBit - bit) + (fromIntegral b)) `mod` 8)})

takeHeadM :: Source -> (Maybe Char, Source)
takeHeadM (s @ Source{current,loc, ..}) =
  if B.null current then (Nothing, s)
  else (Just $ word8ToChr $ B.head current, s{current = B.tail current, loc = incOffset loc})

-- | If the front of the current source input matches the given string then
-- remove it and return the modified source. Otherwise return the original
-- source and a boolean flag indicating that we failed to take the given string
-- off the front of the source input.
takeHeadStr :: String -> Source -> (Bool, Source)
takeHeadStr str s =
   let pstr = strToByteString str
   in if B.isPrefixOf pstr (current s)
      then let (res,source) = Language.Pads.Source.take (B.length pstr) s
            in (True, source)
      else (False, s)


matchString :: String -> Source -> Maybe(String, Source)
matchString str s =
   let pstr = strToByteString str
   in if B.isPrefixOf pstr (current s)
      then let (res,source) = Language.Pads.Source.take (B.length pstr) s
            in Just(str, source)
      else Nothing


breakSubstring :: B.ByteString -- ^ String to search for
               -> B.ByteString -- ^ String to search in
               -> (B.ByteString,B.ByteString) -- ^ Head and tail of string broken at substring
breakSubstring pat src = search 0 src
  where
    -- STRICT2(search)
    search :: Int -> B.ByteString -> (B.ByteString, B.ByteString)
    search a b | a `seq` b `seq` False = undefined
    search n s
        | B.null s             = (src,B.empty)      -- not found
        | pat `B.isPrefixOf` s = (B.take n src,s)
        | otherwise            = search (n+1) (B.tail s)

{-|
  Scan the current source input until we find the given string:
  - If we don't find the string return Nothing and leave source unmodified
  - If we return (Maybe []), then we found the string at the beginning of the
    source and removed it.
  - If we return (Maybe junk), then we found the string somewhere after the
    first character in the source and we consumed / removed (junk:str).
-}
scanStr :: String -> Source -> (Maybe String, Source)
scanStr str (s @ Source{current,loc, ..}) =
  let pat = strToByteString str
      (before,after) = breakSubstring pat current
  in if B.null after then (Nothing, s)
     else let len = B.length pat
          in (Just (byteStringToStr before),
              s{current = B.drop len after, loc = incOffsetBy loc len})


scanString :: String -> Source -> Maybe (String, Source)
scanString str (s @ Source{current,loc, ..}) =
  let pat = strToByteString str
      (before,after) = breakSubstring pat current
  in if B.null after then Nothing
     else let len = B.length pat
          in Just (byteStringToStr before, s{current= B.drop len after, loc = incOffsetBy loc len})

satisfyNB :: (Char -> Bool) -> Source -> (String, Source)
satisfyNB p s =
    let (c, s') = takeBits8 8 s
        c' = word8ToChr c
    in  if   p c'
        then (c' : (fst $ satisfyNB p s'), snd $ satisfyNB p s')
        else ([], s)

takeBytes :: Int -> Source -> (B.ByteString, Source)
takeBytes n (s @ Source{current,loc, ..}) =
     let (head, tail) = B.splitAt n current
         incOffset    = B.length head
     in (head, s{current= tail, loc = incOffsetBy loc incOffset})

takeBytesNB :: Int -> Source -> (B.ByteString, Source)
takeBytesNB n s =
    let (bits, s') = takeBits (n * 8) s
        numToBS x = B.pack $ numToWord8s x []
    in (numToBS bits, s')

numToWord8s :: Integral a => a -> [Word8] -> [Word8]
numToWord8s x accum
    | x < 256   = fromIntegral x : accum
    | otherwise = numToWord8s (x `div` 256) (fromIntegral (x `mod` 256) : accum)

take :: Int -> Source -> (String, Source)
take n s = let (bs, s') = takeBytes n s
           in (byteStringToStr bs, s')

-- | Match the beginning of the source input with a regex, returning a tuple of
-- the matched string and the modified source with that string removed.
regexMatch :: RE -> Source -> (Maybe String, Source)
regexMatch (RE re_str_raw) (s @ Source{current,loc,..}) =
     let (before, match, after) = current TRP.=~ (strToByteString('^' : re_str_raw))
     in if not (B.null before) then (Nothing, s)   -- only looking for matches at the beginning of the string
        else  (Just (byteStringToStr match), s{current=after, loc=incOffsetBy loc (fromIntegral (B.length match))})
regexMatch (REd re_str_raw def ) s = regexMatch (RE re_str_raw) s

-- | Find the first match of a regex in the source input, returning the contents
-- of the source input *before* the match.
-- * If there's no match return Nothing and leave the source unmodified.
-- * If there's a match, return the string before the match and remove *just*
-- the string before from the source input.
regexStop :: RE -> Source -> (Maybe String, Source)
regexStop (RE re_str_raw) (s @ Source{current,loc,..}) =
     let packed = strToByteString re_str_raw
         (before, match, after) = current TRP.=~ packed      -- Is there a way to test this result matches w/o duplicating match?
         isMatch = current TRP.=~ packed
     in if not isMatch
         then (Nothing, s)        -- match failed, return input unchanged
         else (Just (byteStringToStr before),
                s{current= B.append match after,loc=incOffsetBy loc (fromIntegral (B.length before))})

-- | See 'regexStop'
regexStop (REd re_str_raw def) s = regexStop (RE re_str_raw) s

-- | Remove and return the longest prefix of the source input satisfying the
-- given predicate.
span p (s @ Source{current,loc,..}) =
     let (head, tail) = B.span p current
         incOffset    = B.length head
     in (B.unpack head, s{current=tail, loc = incOffsetBy loc incOffset})

-- | Same as 'span' but for predicates over type 'Char'.
whileS :: (Char -> Bool) -> Source -> Maybe (String,Source)
whileS p (s @ Source{current,loc,..}) =
     let (head, tail) = B.span (p . word8ToChr) current
         incOffset    = B.length head
     in Just (byteStringToStr head, s{current=tail, loc=incOffsetBy loc incOffset})

-- | Remove the first byte of the input source.
tail  (s @ Source{current,loc,..}) =
       (s{current=B.tail current,loc=incOffset loc})

-- | Scan the input source until we find the given character. If we don't find
-- the character indicate as such with the boolean (False) and remove all source
-- input from the current record. If we do find the character, return True and
-- consume input up to and including the matched character. The 'Span' in the
-- returned tuple indicates the region in the input that got scanned and removed
-- by this function (whether or not we failed to find the character).
scanTo :: Char -> Source -> (Bool, Source, Span)
scanTo chr (src @ Source{current,loc, ..}) =
     let begin = getSrcLoc src
         (skipped, residual) = B.break (\c->c== (chrToWord8 chr)) current
         (found,remaining,incAmount) =
            if B.null residual then   -- Reached EOR w/o finding chr
                 (False, residual,         B.length skipped)
            else (True,  B.tail residual, (B.length skipped) + 1)
         newLoc = incOffsetBy loc incAmount
         endErrLoc = incOffsetBy loc (B.length skipped)
      in (found,
          src {current = remaining, loc=newLoc},
          Span   {begin, end=Just endErrLoc})


lift :: (String -> [(a, String)]) -> (Source -> (Maybe a, Source))
lift f s = case f (byteStringToStr $ current s) of
  [] -> (Nothing, s)
  (x,residual):rest -> (Just x, s{current= (strToByteString residual)})

{- Helper routines -}

eqCurrent :: Source -> Source -> Bool
eqCurrent s s'= current s == current s'

chrToWord8 :: Char -> Word8
chrToWord8 c = toEnum $ fromEnum c

strToWord8s :: String -> [Word8]
strToWord8s = map chrToWord8

word8ToChr :: Word8 -> Char
word8ToChr = toEnum . fromEnum

word8sToStr :: [Word8] -> String
word8sToStr = map word8ToChr

byteStringToStr :: B.ByteString -> String
byteStringToStr = word8sToStr . B.unpack

strToByteString :: String -> B.ByteString
--strToByteString = B.pack . strToWord8s
strToByteString = Char8.pack



locToSpan :: Loc -> Span
locToSpan loc = Span { begin = loc, end = Nothing }

locsToSpan :: Loc -> Loc -> Span
locsToSpan b e = Span {begin = b, end = Just e}

-------------------------------------------------------------------------------
-- * Pretty print sources
instance Pretty Source where
    ppr (Source{current, rest, ..}) = text "Current:" <+> text (show current)

instance Pretty Loc where
 ppr (Loc{recordNumber,byteOffset}) = text "Line:" <+> ppr recordNumber <> text ", Offset:" <+> ppr byteOffset

instance Pretty Span where
  ppr (Span{begin,end}) = case end of
                                Nothing -> ppr begin
                                Just end_loc ->  text "from:" <+> ppr begin <+> text "to:" <+> ppr end_loc
