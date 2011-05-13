{-# LANGUAGE NamedFieldPuns, RecordWildCards, DeriveDataTypeable #-}

{-
** *********************************************************************
*                                                                      *
*              This software is part of the pads package               *
*           Copyright (c) 2005-2011 AT&T Knowledge Ventures            *
*                      and is licensed under the                       *
*                        Common Public License                         *
*                      by AT&T Knowledge Ventures                      *
*                                                                      *
*                A copy of the License is available at                 *
*                    www.padsproj.org/License.html                     *
*                                                                      *
*  This program contains certain software code or other information    *
*  ("AT&T Software") proprietary to AT&T Corp. ("AT&T").  The AT&T     *
*  Software is provided to you "AS IS". YOU ASSUME TOTAL RESPONSIBILITY*
*  AND RISK FOR USE OF THE AT&T SOFTWARE. AT&T DOES NOT MAKE, AND      *
*  EXPRESSLY DISCLAIMS, ANY EXPRESS OR IMPLIED WARRANTIES OF ANY KIND  *
*  WHATSOEVER, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF*
*  MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE, WARRANTIES OF  *
*  TITLE OR NON-INFRINGEMENT.  (c) AT&T Corp.  All rights              *
*  reserved.  AT&T is a registered trademark of AT&T Corp.             *
*                                                                      *
*                   Network Services Research Center                   *
*                          AT&T Labs Research                          *
*                           Florham Park NJ                            *
*                                                                      *
*              Kathleen Fisher <kfisher@research.att.com>              *
*                                                                      *
************************************************************************
-}

module Language.Pads.Source where
import qualified Data.ByteString.Lazy.Char8 as B   -- abstraction for input data
import qualified Text.Regex.Posix as TRP
import Language.Pads.RegExp                        -- user-specified regular expressions
import Text.PrettyPrint.Mainland as PP 

import Data.Int
import Data.Data
import Data.Word
import Data.Char


type RawStream = B.ByteString

{- Input source abstraction -}
data Loc = Loc { lineNumber :: Int64,
                 byteOffset :: Int64 }
     deriving (Typeable, Data,Eq, Ord)


data Source = Source {current  :: B.ByteString,
                      rest     :: B.ByteString,
                      atEOF    :: Bool,
                      loc      :: Loc }

data Pos = Pos { begin      :: Loc,
                 end        :: Maybe Loc}
  deriving (Typeable, Data, Eq, Ord)

locToPos :: Loc -> Pos
locToPos loc = Pos { begin = loc, end = Nothing }

locsToPos :: Loc -> Loc -> Pos
locsToPos b e = Pos {begin = b, end = Just e}

instance Pretty Loc where
 ppr (Loc{lineNumber,byteOffset}) = text "Line:" <+> PP.ppr lineNumber <> text ", Offset:" <+> PP.ppr byteOffset 

instance Pretty Pos where 
  ppr (Pos{begin,end}) = case end of
                                Nothing -> PP.ppr begin
                                Just end_loc ->  text "from:" <+> PP.ppr begin <+> text "to:" <+> PP.ppr end_loc

instance Pretty Source where 
    ppr (Source{current, rest, ..}) = text "Current:" <+> text (show current)
                                

chrToWord8 :: Char -> Word8
chrToWord8 c = toEnum $ fromEnum c

word8ToChr :: Word8 -> Char 
word8ToChr w = toEnum $ fromEnum w

{- Called when current is empty.
   Should not be called when atEOF is already set. -}
getNextLine_newline (s @ Source {current, rest, atEOF, loc = Loc{lineNumber, byteOffset}}) = 
      if atEOF then s
      else if B.null rest then
            (Source {current = B.empty, rest = rest, atEOF = True, loc = Loc{lineNumber, byteOffset=0}})
      else  (Source {current = nextLine, rest=residual, atEOF = False, loc = Loc{lineNumber=lineNumber+1, byteOffset=0}})
        where (nextLine, raw_residual) = B.break (\c->c == '\n') rest
              residual = B.drop 1 raw_residual

padsSourceFromByteString :: B.ByteString -> Source
padsSourceFromByteString bs = 
             getNextLine_newline (Source{current = B.empty,
                                         rest    = bs,
                                         atEOF   = False,   -- if string is empty, will be made True by getNextLine_newline 
                                         loc     = Loc{ lineNumber = -1,   -- will be incremeneted to 0 by getNextLine_newline
                                                        byteOffset = 0}})


padsSourceFromString str = padsSourceFromByteString (B.pack str)

-- XXX unclear that this is fully correct.  When do we need to insert a newline????
padsSourceToString (Source {current, rest, ..}) = 
  if B.null rest then B.unpack current
  else B.unpack (B.concat [current,B.pack ['\n'], rest])

drainSource :: Source -> (String, Source)
drainSource (s @ Source {current, rest, atEOF, loc=loc_orig}) = (padsSourceToString s,   
                                                        Source {current = B.empty, rest = B.empty, atEOF = True, loc = loc_orig})

rawSource :: Source -> (B.ByteString, Source) 
rawSource (s @ Source {current, rest, atEOF, loc=loc_orig}) = (B.concat [current,rest],
                                                        Source {current = B.empty, rest = B.empty, atEOF = True, loc = loc_orig})

isEOF (Source{current,rest,..}) = B.null current && B.null rest
isEOR (Source{current,..}) = B.null current

{- Return the rest of the current record as a string -}
restRec :: Source -> String
restRec (s @ Source {current, rest, atEOF, loc=loc_orig}) = B.unpack current





head  (Source{current,..}) = B.head current 
takeHead (Source{current,rest,atEOF,loc=Loc{byteOffset,lineNumber}}) = 
       (B.head current, Source{current=B.tail current,rest,atEOF,loc=Loc{byteOffset=byteOffset+1,lineNumber}})


matchString :: String -> Source -> Maybe(String, Source)
matchString str (s @ Source{current,rest,atEOF,loc=Loc{byteOffset,lineNumber}}) = 
   let pstr = B.pack str 
   in if B.isPrefixOf pstr current
      then let (res,source) = Language.Pads.Source.take (B.length pstr) s
            in Just(str, source)            
      else Nothing

takeHeadStr :: String -> Source -> (Bool, Source)
takeHeadStr str (s @ Source{current,rest,atEOF,loc=Loc{byteOffset,lineNumber}}) = 
   let pstr = B.pack str 
   in if B.isPrefixOf pstr current
      then let (res,source) = Language.Pads.Source.take (B.length pstr) s
            in (True, source)            
      else (False, s)


breakSubstring :: B.ByteString -- ^ String to search for
               -> B.ByteString -- ^ String to search in
               -> (B.ByteString,B.ByteString) -- ^ Head and tail of string broken at substring
breakSubstring pat src = search 0 src
  where
    -- STRICT2(search)
    search :: Int64 -> B.ByteString -> (B.ByteString, B.ByteString)
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
scanStr str (s @ Source{current,rest,atEOF,loc=Loc{byteOffset,lineNumber}}) = 
  let pat = B.pack str
      (before,after) = breakSubstring pat current
  in if B.null after then (Nothing, s)
     else let len = B.length pat
          in (Just (B.unpack before), Source{current= B.drop len after, rest, atEOF,loc =Loc{byteOffset=byteOffset+len,lineNumber}})

scanString :: String -> Source -> Maybe (String, Source)
scanString str (s @ Source{current,rest,atEOF,loc=Loc{byteOffset,lineNumber}}) = 
  let pat = B.pack str
      (before,after) = breakSubstring pat current
  in if B.null after then Nothing
     else let len = B.length pat
          in Just (B.unpack before, Source{current= B.drop len after, rest, atEOF,loc =Loc{byteOffset=byteOffset+len,lineNumber}})

                

take n (Source{current,rest,atEOF,loc=Loc{byteOffset,lineNumber}}) = 
     let (head, tail) = B.splitAt n current
         newOffset    = byteOffset + (B.length head)
     in (B.unpack head, Source{current=tail,rest,atEOF,loc=Loc{byteOffset=byteOffset+newOffset,lineNumber}})

take' :: Int64  -> Source -> Maybe (String, Source)
take' n (Source{current,rest,atEOF,loc=Loc{byteOffset,lineNumber}}) = 
     if n > B.length current then Nothing
     else let (head, tail) = B.splitAt n current
              newOffset    = byteOffset + (B.length head)
          in Just (B.unpack head, Source{current=tail,rest,atEOF,loc=Loc{byteOffset=byteOffset+newOffset,lineNumber}})


regexMatch (RE re_str_raw) (s @ Source{current,rest,atEOF,loc=Loc{byteOffset,lineNumber}}) = 
     let (before, match, after) = current TRP.=~ (B.pack('^' : re_str_raw))
     in if not (B.null before) then (Nothing, s)   -- only looking for matches at the beginning of the string
        else  (Just (B.unpack match), Source{current= after,rest,atEOF,loc=Loc{byteOffset=byteOffset+(fromIntegral (B.length match)),lineNumber}})
regexMatch (REd re_str_raw def ) s = regexMatch (RE re_str_raw) s


regexStop (RE re_str_raw) (s @ Source{current,rest,atEOF,loc=Loc{byteOffset,lineNumber}}) = 
     let packed = B.pack re_str_raw
         (before, match, after) = current TRP.=~ packed      -- Is there a way to test this result matches w/o duplicating match?
         isMatch = current TRP.=~ packed
     in if not isMatch
         then (Nothing, s)        -- match failed, return input unchanged
         else (Just (B.unpack before), 
                Source{current= B.append match after,rest,atEOF,loc=Loc{byteOffset=byteOffset+(fromIntegral (B.length before)),lineNumber}})
regexStop (REd re_str_raw def) s = regexStop (RE re_str_raw) s


span p (Source{current,rest,atEOF,loc=Loc{byteOffset,lineNumber}}) = 
     let (head, tail) = B.span p current
         newOffset    = byteOffset + (B.length head)
     in (B.unpack head, Source{current=tail,rest,atEOF,loc=Loc{byteOffset=byteOffset+newOffset,lineNumber}})

whileS :: (Char -> Bool) -> Source -> Maybe (String,Source)
whileS p (Source{current,rest,atEOF,loc=Loc{byteOffset,lineNumber}}) = 
     let (head, tail) = B.span p current
         newOffset    = byteOffset + (B.length head)
     in Just (B.unpack head, Source{current=tail,rest,atEOF,loc=Loc{byteOffset=byteOffset+newOffset,lineNumber}})

tail  (Source{current,rest,atEOF,loc=Loc{byteOffset,lineNumber}}) = 
       (Source{current=B.tail current,rest,atEOF,loc=Loc{byteOffset=byteOffset+1,lineNumber}})

getSrcLoc (Source {loc, ..}) = loc

scanTo chr (src @ Source{current,rest,atEOF,loc=Loc{byteOffset,lineNumber}}) = 
     let begin = getSrcLoc src
         (skipped, residual) = B.break (\c->c==chr) current
         (found,remaining,newByteOffset) =   
            if B.null residual then   -- Reached EOR w/o finding chr
                 (False, residual,        byteOffset + (B.length skipped))
            else (True,  B.tail residual, byteOffset + (B.length skipped) + 1)
         newLoc    = Loc{lineNumber, byteOffset = newByteOffset}
         endErrLoc = Loc{lineNumber, byteOffset = byteOffset + (B.length skipped)}
      in (found, 
          Source {current = remaining, rest, atEOF, loc=newLoc},
          Pos    {begin, end=Just endErrLoc})

eqCurrent :: Source -> Source -> Bool
eqCurrent s s'= current s == current s'

srcLineBegin :: Source -> (Maybe String, Source)
srcLineBegin s = (Nothing, s)

srcLineEnd :: Source -> (Maybe String, Source)
srcLineEnd s = 
  if atEOF s then (Just "Found EOF when looking for EOR", s)
             else (Nothing, getNextLine_newline s)

{-
printEOR :: B.ByteString
printEOR = B.pack "\n"

printEOF:: B.ByteString
printEOF = B.pack []
-}