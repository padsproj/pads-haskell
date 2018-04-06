{-# LANGUAGE TypeFamilies, ScopedTypeVariables, DeriveDataTypeable
           , MultiParamTypeClasses
           , TypeSynonymInstances
           , TemplateHaskell
           , QuasiQuotes
           , FlexibleInstances #-}
module Main where
import System.IO.Unsafe (unsafePerformIO)
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import qualified Test.HUnit as H
import Test.HUnit ((@?=))
import Test.QuickCheck (Property, quickCheck, (==>))
import qualified Test.QuickCheck.Monadic as TQM
import Control.Monad.State.Lazy (liftIO)

import Language.Pads.Padsc
import Proc
import qualified First as First
import qualified AI as AI
import qualified Binary as Binary

procMaps fn =
  let (MapsFile maps, maps_md) = unsafePerformIO $ parseFileWith mapsFile_parseM fn
  in  last maps
      @?=
      Region { start_addr = "ffffffffff600000"
              , end_addr   = "ffffffffff601000"
              , perms      = Permissions
                { permRead   = READ
                , permWrite  = NOWRITE
                , permExec   = EXEC
                , permShare  = PRIVATE
                }
              , offset     = 0
              , device     = ("00", "00")
              , inode      = 0
              , path       = VSyscall
      }

testFirst =
  unsafePerformIO First.test
  @?=
  H.Counts (length First.tests) (length First.tests) 0 0

testAI = True ==> TQM.monadicIO $ do
  r <- liftIO $ AI.result 1
  TQM.assert $ r ==
    [ AI.Entry
        { AI.host = AI.Addr 207 136 97 49
        , AI.identdID = AI.Missing
        , AI.httpID   = AI.Missing
        , AI.time     = read "1997-10-16 01:46:51 UTC"
        , AI.request  = AI.Request
          { AI.method = AI.GET
          , AI.url    = "/turkey/amnty1.gif"
          , AI.version = AI.Version {AI.major = 1, AI.minor = 0}
          }
        , AI.response = 200
        , AI.contentLen = AI.ContentLength 3013
        }
    ]

testBinary =
  unsafePerformIO Binary.test
  @?=
  H.Counts (length Binary.tests) (length Binary.tests) 0 0

main :: IO ()
main = defaultMainWithOpts
  [ testCase "First"   testFirst
  , testProperty "AI"  testAI
  , testCase "proc maps file 0" (procMaps "examples/data/maps0")
  ] mempty

