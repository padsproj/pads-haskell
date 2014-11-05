module Language.Pads.Quote
    (pads)
    where

import Prelude hiding (exp, init)
import System.IO.Unsafe (unsafePerformIO)

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Language.Pads.CodeGen (make_pads_declarations)
import qualified Language.Pads.Parser as P


{-
  The PADS quasi-quoter only has declaration forms, so the forms
  for expressions, patterns, and types should not arise.
-}
pads :: QuasiQuoter
pads  = QuasiQuoter (error "parse expression")
                    (error "parse pattern")
                    (error "parse type")
                    pparse

pparse :: String -> TH.Q [TH.Dec]
pparse input = do
    loc <- TH.location
    let fileName = TH.loc_filename loc
    let (line,column) = TH.loc_start loc
    case P.parsePadsDecls fileName line column input of
      Left err -> unsafePerformIO $ fail $ show err
      Right x  -> make_pads_declarations x



