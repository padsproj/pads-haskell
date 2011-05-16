{-
** *********************************************************************
*                                                                      *
*         (c)  Kathleen Fisher <kathleen.fisher@gmail.com>             *
*              John Launchbury <john.launchbury@gmail.com>             *
*                                                                      *
************************************************************************
-}


module Language.Pads.Testing (
    module Test.HUnit ,
    mkTestCase,
    mkFileTestCase,
    mdToError,
    mdFileToError
  )
  where

import Language.Pads.Padsc
import Test.HUnit hiding (test)

mdToError ((rep,md), residual) = (rep, getTotalErrors md, residual)
mkTestCase s expected seen = TestCase(assertEqual s expected  (mdToError seen))

mdFileToError (rep,md) = (rep, getTotalErrors md)
mkFileTestCase s expected seen = TestCase(assertEqual s expected (mdFileToError seen))


getTotalErrors :: PadsMD md => md -> Int
getTotalErrors md = numErrors $ get_md_header md
