{-# LANGUAGE NamedFieldPuns, RecordWildCards, DeriveDataTypeable #-}

{-
** *********************************************************************
*                                                                      *
*         (c)  Kathleen Fisher <kathleen.fisher@gmail.com>             *
*              John Launchbury <john.launchbury@gmail.com>             *
*                                                                      *
************************************************************************
-}

module Language.Pads.Source where

import qualified Data.ByteString as B   -- abstraction for input data
import qualified Text.Regex.Posix as TRP
import Language.Pads.RegExp                        -- user-specified regular expressions
import Text.PrettyPrint.Mainland as PP 

import Data.Int
import Data.Data
import Data.Word
import Data.Char

type RawStream = B.ByteString   -- This is the type that should be used in other files!!!

{- Input source abstraction -}
data Source = Source { current  :: B.ByteString
                     , rest     :: B.ByteString
                     , loc      :: Loc
                     , disc     :: RecordDiscipline
                     , eorAtEOF :: Bool  -- Relevant for seperator-based record disciplines: Single, Multi
                                         -- Set when current record is last record and separtor appeared at end.
                     }    

data RecordDiscipline = Single Word8
                      | Multi B.ByteString
                      | Bytes Int
                      | NoPartition
                      | NoDiscipline  -- No discipline is currently installed; all input data is in 'rest' field

newline = Single (chrToWord8 '\n')
windows = Multi  (B.pack (strToWord8s "\r\n"))
bytes n = Bytes n
none = NoPartition

{- SOURCE LOCATIONS -}
data Loc = Loc { lineNumber :: Int64,
                 byteOffset :: Int64 }
     deriving (Typeable, Data,Eq, Ord, Show)

data Pos = Pos { begin      :: Loc,
                 end        :: Maybe Loc}
  deriving (Typeable, Data, Eq, Ord, Show)

zeroLoc   = Loc {lineNumber = 0,  byteOffset = 0}

incLineNumber :: Loc -> Loc
incLineNumber Loc{lineNumber, ..} = Loc{ lineNumber = lineNumber+1
                                       , byteOffset = 0}
decLineNumber :: Loc -> Loc
decLineNumber Loc{lineNumber, ..} = Loc{lineNumber=lineNumber-1, byteOffset=0}

incOffset :: Loc -> Loc
incOffset Loc{lineNumber, byteOffset} = Loc{ lineNumber = lineNumber
                                           , byteOffset = byteOffset + 1 }

incOffsetBy :: Loc -> Int -> Loc
incOffsetBy Loc{lineNumber, byteOffset} n = Loc{ lineNumber = lineNumber
                                               , byteOffset = byteOffset + (fromIntegral n) }

decOffset :: Loc -> Loc
decOffset Loc{lineNumber, byteOffset} = Loc{ lineNumber = lineNumber
                                           , byteOffset = byteOffset - 1 }

getSrcLoc :: Source -> Loc
getSrcLoc = loc

getRecordDiscipline :: Source -> RecordDiscipline
getRecordDiscipline = disc



{- SOURCE CREATION -}
emptySource = Source {current = B.empty, rest = B.empty, loc = zeroLoc, eorAtEOF = False, disc = NoDiscipline}

padsSourceFromString :: String -> Source
padsSourceFromString str = padsSourceFromByteString (strToByteString str)

padsSourceFromFile :: FilePath -> IO Source
padsSourceFromFile file = do
  { bs <- B.readFile file
  ; return (padsSourceFromByteString bs)
  }

padsSourceFromByteString :: B.ByteString -> Source
padsSourceFromByteString bs = 
    let rawSource = Source{ current  = B.empty
                          , rest     = bs
                          , loc      = zeroLoc  
                          , disc     = newline 
                          , eorAtEOF = False  
                          }
    in getNextLine rawSource

isEOF :: Source -> Bool
isEOF (s @ Source{current, rest, eorAtEOF, ..}) = B.null current && B.null rest && not eorAtEOF

isEOR :: Source -> Bool
isEOR = B.null . current


{- RECORD MANIPULATING FUNCTIONS -}
{- Called when current is empty to get the next record, where the disc field defines what constitutes a record.
   NOOP when isEOF is already true. -}
getNextLine :: Source -> Source
getNextLine (s @ Source {current, rest, loc, disc, eorAtEOF}) = 
      if isEOF s then s
      else if eorAtEOF || B.null rest then
            (Source {current = B.empty, rest = B.empty, loc = incLineNumber loc, disc, eorAtEOF = False})
      else  (Source {current = nextLine, rest=residual, loc = incLineNumber loc, disc, eorAtEOF = eorAtEOF'})
        where (nextLine, residual, eorAtEOF') = breakUsingDisc rest disc

srcLineBegin :: Source -> (Maybe String, Source)
srcLineBegin s = (Nothing, s)

srcLineEnd :: Source -> (Maybe String, Source)
srcLineEnd s = if isEOF s 
     then (Just "Found EOF when looking for EOR", s)
     else (Nothing, getNextLine s)

{- External code should not set discipline to NoDiscipline; that is an internal state only to 
   mark state between two legal external disciplines. -}
setRecordDiscipline :: RecordDiscipline -> Source -> ((),Source)
setRecordDiscipline r s = 
  let s'   = unputCurrentLine s
      s'' = s'{disc = r}
  in ((),getNextLine s'')

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
                    in Source {current = B.empty, rest = rest', loc = loc', disc = NoDiscipline, eorAtEOF = False}
        Multi  br -> let rest' = if B.null rest 
                                then if eorAtEOF 
                                     then B.concat [current, br]
                                     else current
                                else B.concat [current, rest]
                         loc'  = if B.null current then loc else decLineNumber loc
                    in Source {current = B.empty, rest = rest', loc = loc', disc = NoDiscipline, eorAtEOF = False}
        Bytes n -> Source {current = B.empty, rest = B.append current rest, loc = decLineNumber loc, disc = NoDiscipline, eorAtEOF = False}            
        NoPartition -> Source {current = B.empty, rest = current, loc = decLineNumber loc, disc = NoDiscipline, eorAtEOF = False}            
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

             




{- CONVERTING SOURCES TO STRINGS -}
padsSourceToString :: Source -> String
padsSourceToString = (map word8ToChr) . B.unpack . padsSourceToByteString

padsSourceToByteString :: Source -> B.ByteString
padsSourceToByteString = rest . unputCurrentLine

drainSource :: Source -> (String, Source)
drainSource s = (padsSourceToString s, emptySource)

{- XXX: Change call site to use no record discipline; then this call should just became an instance of drainSource -}
rawSource :: Source -> (B.ByteString, Source) 
rawSource s = (B.concat [current s ,rest s], emptySource)

{- Return the rest of the current record as a string -}
restRec :: Source -> String
restRec = byteStringToStr . current 



{- OPERATIONS WITHIN A SINGLE RECORD -}
head :: Source -> Char
head = word8ToChr . headOrZero . current 

headOrZero s = if B.null s then chrToWord8 '\0' else B.head s

peekHeadM :: Source -> (Maybe Char, Source)
peekHeadM (s @ Source{current,loc, ..}) = 
  if B.null current then (Nothing, s) else (Just (Language.Pads.Source.head s), s)

takeHead :: Source -> (Char, Source)
takeHead (s @ Source{current,loc, ..}) = 
         (word8ToChr $ B.head current, s{current = B.tail current, loc = incOffset loc})

takeHeadM :: Source -> (Maybe Char, Source)
takeHeadM (s @ Source{current,loc, ..}) = 
  if B.null current then (Nothing, s) 
  else (Just $ word8ToChr $ B.head current, s{current = B.tail current, loc = incOffset loc})


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


{- 
  Nothing  = didn't find string; source is unaffected
  Maybe [] = matched immediately; source advanced over matched string
  Maybe junk = matched after finding str; source advanced over junk and str
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

takeBytes :: Int -> Source -> (B.ByteString, Source)
takeBytes n (s @ Source{current,loc, ..}) = 
     let (head, tail) = B.splitAt n current
         incOffset    = B.length head
     in (head, s{current= tail, loc = incOffsetBy loc incOffset})


take :: Int -> Source -> (String, Source)                
take n s = let (bs, s') = takeBytes n s
           in (byteStringToStr bs, s')


regexMatch :: RE -> Source -> (Maybe String, Source)
regexMatch (RE re_str_raw) (s @ Source{current,loc,..}) = 
     let (before, match, after) = current TRP.=~ (strToByteString('^' : re_str_raw))
     in if not (B.null before) then (Nothing, s)   -- only looking for matches at the beginning of the string
        else  (Just (byteStringToStr match), s{current=after, loc=incOffsetBy loc (fromIntegral (B.length match))})
regexMatch (REd re_str_raw def ) s = regexMatch (RE re_str_raw) s

regexStop :: RE -> Source -> (Maybe String, Source)
regexStop (RE re_str_raw) (s @ Source{current,loc,..}) = 
     let packed = strToByteString re_str_raw
         (before, match, after) = current TRP.=~ packed      -- Is there a way to test this result matches w/o duplicating match?
         isMatch = current TRP.=~ packed
     in if not isMatch
         then (Nothing, s)        -- match failed, return input unchanged
         else (Just (byteStringToStr before), 
                s{current= B.append match after,loc=incOffsetBy loc (fromIntegral (B.length before))})

regexStop (REd re_str_raw def) s = regexStop (RE re_str_raw) s


span p (s @ Source{current,loc,..}) = 
     let (head, tail) = B.span p current
         incOffset    = B.length head
     in (B.unpack head, s{current=tail, loc = incOffsetBy loc incOffset})

whileS :: (Char -> Bool) -> Source -> Maybe (String,Source)
whileS p (s @ Source{current,loc,..}) = 
     let (head, tail) = B.span (p . word8ToChr) current
         incOffset    = B.length head
     in Just (byteStringToStr head, s{current=tail, loc=incOffsetBy loc incOffset})

tail  (s @ Source{current,loc,..}) = 
       (s{current=B.tail current,loc=incOffset loc})

scanTo :: Char -> Source -> (Bool, Source, Pos)
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
          Pos    {begin, end=Just endErrLoc})


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
strToByteString = B.pack . strToWord8s



locToPos :: Loc -> Pos
locToPos loc = Pos { begin = loc, end = Nothing }

locsToPos :: Loc -> Loc -> Pos
locsToPos b e = Pos {begin = b, end = Just e}


{- Pretty print sources -}
instance Pretty Source where 
    ppr (Source{current, rest, ..}) = text "Current:" <+> text (show current)

instance Pretty Loc where
 ppr (Loc{lineNumber,byteOffset}) = text "Line:" <+> PP.ppr lineNumber <> text ", Offset:" <+> PP.ppr byteOffset 

instance Pretty Pos where 
  ppr (Pos{begin,end}) = case end of
                                Nothing -> PP.ppr begin
                                Just end_loc ->  text "from:" <+> PP.ppr begin <+> text "to:" <+> PP.ppr end_loc


