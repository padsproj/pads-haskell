{-# LANGUAGE TypeFamilies, ScopedTypeVariables, DeriveDataTypeable
           , MultiParamTypeClasses
           , TypeSynonymInstances
           , TemplateHaskell
           , QuasiQuotes
           , FlexibleInstances #-}
-- :set -ddump-splices
module Examples.Proc where
import Language.Pads.Padsc
import Language.Pads.GenPretty

ws = RE "[ \t]+"

[pads|

	newtype MapsFile = MapsFile ([Line Region] terminator EOF)
	
  data Region = Region
    {           start_addr :: Hex
    , '-',      end_addr   :: Hex
    , ' ',      perms      :: Permissions
    , ' ',      offset     :: Int
    , ' ',      device     :: (Hex, ':', Hex)
    , ' ',      inode      :: Int
    , ws ,      path       :: RegionName
    }

  type Hex = StringME '[0-9A-Fa-f]+'

  data RegionName =
      Heap      "[heap]"
    | Stack     "[stack]"
    | VDSO      "[vdso]"
    | VVAR      "[vvar]"
    | VSyscall  "[vsyscall]"
    | Path      ([Char] terminator EOR)
    | Anonymous ""

  data Permissions = Permissions
    { permRead  :: RP
    , permWrite :: WP
    , permExec  :: XP
    , permShare :: SP
    }

  data RP = READ  'r' | NOREAD  '-'
  data WP = WRITE 'w' | NOWRITE '-'
  data XP = EXEC  'x' | NOEXEC  '-'
  data SP = SHARE 's' | PRIVATE 'p'

|]

