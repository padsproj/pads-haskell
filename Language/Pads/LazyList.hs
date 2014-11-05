{-
** *********************************************************************
*                                                                      *
*         (c)  Kathleen Fisher <kathleen.fisher@gmail.com>             *
*              John Launchbury <john.launchbury@gmail.com>             *
*                                                                      *
************************************************************************
-}

module Language.Pads.LazyList where

import qualified Data.ByteString as B
import qualified Language.Pads.Source as S
import qualified Data.List as List

-- Lazy append-lists 

type FList = B.ByteString -> B.ByteString

(+++) :: FList -> FList -> FList 
p +++ q = \ws -> p (q ws)

nil :: FList 
nil = \ws -> ws

concatFL :: [FList] -> FList
concatFL (f:fs) = f +++ (concatFL fs)
concatFL [] = nil

printNothing :: FList
printNothing ws = ws

addBString :: B.ByteString -> FList 
addBString bs = \ws -> B.append bs  ws

addString :: String -> FList 
addString s = \ws -> B.append (S.strToByteString s)  ws

fshow :: Show a => a -> FList 
fshow x = \ws -> B.append (S.strToByteString (show x)) ws


printEOR :: FList
printEOR = addString ['\n']

printEOF :: FList
printEOF = addString []

endRecord :: FList -> FList
endRecord fst = fst +++ printEOR


printF :: FList -> IO ()
printF q = Prelude.print (B.unpack (q B.empty))



--------------------------------------------------



printList :: ((r,m) -> FList) -> FList -> FList -> ([r], (b,[m])) -> FList
printList printItem printSep printTerm (reps, (_,mds)) = 
   (concatFL (List.intersperse printSep (map printItem (zip reps mds))) )
   +++ printTerm










