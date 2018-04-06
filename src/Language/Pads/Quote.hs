{-|
  Module      : Language.Pads.Quote
  Description : Quasiquoter
  Copyright   : (c) 2005-2011 AT&T
                Kathleen Fisher <kfisher@research.att.com>
  License     : MIT
  Maintainer  : Karl Cronburg <karl@cs.tufts.edu>
  Stability   : experimental

-}
module Language.Pads.Quote
    (pads, padsDerivation, pparseDecl, make_pads_declarations)
    where

import Prelude hiding (exp, init)
import System.IO.Unsafe (unsafePerformIO)

import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Language.Pads.CodeGen
import qualified Language.Pads.Parser as P

-- | The PADS quasiquoter which can be invoked by e.g.:
--
-- > [pads| type MyType = (Int, Int) |]
pads :: QuasiQuoter
pads = padsDerivation (const $ return [])

-- | Same as /pads/, but parametrized by a higher order function which
-- constructs a list of Haskell decls to splice into scope for each PADS
-- metadata and data declaration. Namely the /type/, /newtype/, and /data/ PADS
-- constructs get passed into /derivation/ as a template haskell declaration.
--
-- PADS only supports quasiquotes in place of a Haskell declaration
-- (expressions, patterns, and types produce errors).
padsDerivation :: Derivation -> QuasiQuoter
padsDerivation derivation = QuasiQuoter
                    pparseExp
                    (error "parse pattern")
                    (error "parse type")
                    (pparseDecl derivation)

-- | Just the declaration parser for a PADS quasiquotation. Glues together
-- 'P.parsePadsDecls' and 'make_pads_declarations', the parser and code
-- generator.
pparseDecl :: Derivation -> String -> Q [Dec]
pparseDecl derivation input = do
    loc <- location
    let fileName = loc_filename loc
    let (line,column) = loc_start loc
    case P.parsePadsDecls fileName line column input of
      Left err -> unsafePerformIO $ fail $ show err
      Right x  -> make_pads_declarations' derivation x

-- | Just the declaration parser for a PADS quasiquotation. Glues together
-- 'P.parsePadsDecls' and 'make_pads_declarations', the parser and code
-- generator.
pparseExp :: String -> Q Exp
pparseExp input = do
    loc <- location
    let fileName = loc_filename loc
    let (line,column) = loc_start loc
    case P.parsePadsDecls fileName line column input of
      Left err -> unsafePerformIO $ fail $ show err
      Right x  -> make_pads_asts x

