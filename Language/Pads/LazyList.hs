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

--cons :: a -> FList a -> FList a
--cons x q = \ws -> x : q ws

fshow :: Show a => a -> FList 
fshow x = \ws -> B.append (S.strToByteString (show x)) ws

addString :: String -> FList 
addString s = \ws -> B.append (S.strToByteString s)  ws

printEOR :: FList
printEOR = addString ['\n']

printEOF :: FList
printEOF = addString []

printNothing :: FList
printNothing ws = ws

endRecord :: FList -> FList
endRecord fst = fst +++ printEOR

addBString :: B.ByteString -> FList 
addBString bs = \ws -> B.append bs  ws

printF :: FList -> IO ()
printF q = Prelude.print (B.unpack (q B.empty))


printList (reps, (_,mds)) printItem printSep printTerm = 
   (concatFL (List.intersperse printSep (map printItem (zip reps mds))) )
   +++ printTerm



