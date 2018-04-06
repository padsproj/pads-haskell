{-# LANGUAGE TypeFamilies, TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, NamedFieldPuns, ScopedTypeVariables #-}
module HPROF where
import Language.Pads.Padsc
import Language.Pads.Library.LittleEndian as LE
import qualified Language.Pads.Library.BigEndian as BE
import qualified Language.Pads.Library.Native as Native
import Language.Pads.Testing
import System.IO.Unsafe (unsafePerformIO)

-- the format specified in:
-- https://java.net/downloads/heap-snapshot/hprof-binary-format.html

[pads| 
       type U4 = BE.Word32
       type U2 = BE.Word16

       -- total header length: 31 = 19 + 4 + 4 + 4
       data Header = Header
           { version :: [Char] length <| 19 |> 
           , idsiz :: U4
           , time_hw :: U4
           , time_lw :: U4 }
                     -- TODO: can we convert this to the date and hard code it?
                     -- 00 00 01 2b 87 5b 95 3e
                     -- 299 2270926142
                     -- 1286466147646 = October 7th, 2010
                     -- http://www.epochconverter.com/
       data Hprof = Hprof
           { hdr :: Header
           -- , entry :: Entry
           , entries :: Entries -- Originally EntriesB
           , theRest :: [Word8] }
                  
       -- total entry length: 9 + len = 1 + 4 + 4 + len where len is determined by the tag field
       data Entry = Entry
           { tag :: Word8
           , time :: U4
           , len :: U4
             -- fromIntegral :: (Integral a, Num b) => a -> b
           , body :: Body <| (fromIntegral(tag) :: Int, fromIntegral(len) :: Int) |> 
           -- , body2 :: Body <| (fromIntegral(tag) :: Int, fromIntegral(len) :: Int) |>
}

           -- ???: Why does fromIntegral work here?

       -- !!: Want each Entry to take as many bytes as it needs, and the parser starts reading the next byte as a new Entry
       type Entries = [Entry] terminator EOF -- ?? [Entry | EOR] 
       -- type EntriesB = partition Entries using <| bytes 6 |>
       -- ?? Does the list type always have to have corresponding terminator and partition declaration?
           
       type ID = U4 -- !! should take idsiz from the Header

       data Body (tag :: Int, len :: Int) =
         case <| tag |> of
           1 -> One  -- ?? One and Two never declared...
                { oneid :: ID
                , str :: [Char] length <| len - 4 |> } -- !! should be len - idsiz.  len - (idsiz hdr)
         | 2 -> Two 
                { clsserial :: U4
                , objid :: ID 
                , stkserial :: U4
                , clsnamestrid :: ID }
         | 3 -> Three 
                { clsserial :: Word32 }
         | 4 -> Four 
                { stkframeid :: ID
                , method_nameid :: ID
                , method_sigid :: ID
                , srcfile_nameid :: ID
                , clsserial :: Word32
                , stkInfo :: [Word8] } -- !! should take an argument
         | 14 -> Fourteen
                 { bitmask :: U4 -- bizarre case: using 32 bits when you need 1
                 , stkdepth :: U2
                 --, next :: Word8 
                 -- , rest :: [Word8] 
                 }
         | otherwise -> Other [Word8]

-- Limitations:
-- One cannot access Header->idsiz from inside of an Entry type?
-- 

|]

hprof_input_file = "Examples/data/specjbb.hprof"
-- hprof_result :: (Header, Header_md) = unsafePerformIO $ parseFileWith header_parseM hprof_input_file
hprof_result :: (Hprof, Hprof_md) = unsafePerformIO $ parseFileWith hprof_parseM hprof_input_file

-- more readable result
result = fst hprof_result

-- returns first n entries
taken n = Prelude.take n $ entries result

-- gets the nth element. index starts at 1.
getnth n = Prelude.head $ Prelude.drop (n-1) $ entries result

hdrlen = 31
-- returns the number of bytes consumed to read n entries
lenupto n = hdrlen + (foldr (+) 0 $ map ((+9) . len) $ Prelude.take n $ entries result)
