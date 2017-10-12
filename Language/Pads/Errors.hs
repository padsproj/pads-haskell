{-# LANGUAGE NamedFieldPuns, DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK prune #-} 
{-|
  Module      : Language.Pads.Errors
  Description : Parse error reporting support
  Copyright   : (c) 2011
                Kathleen Fisher <kathleen.fisher@gmail.com>
                John Launchbury <john.launchbury@gmail.com>
  License     : MIT
  Maintainer  : Karl Cronburg <karl@cs.tufts.edu>
  Stability   : experimental
-}

module Language.Pads.Errors where
import Text.PrettyPrint.Mainland as PP
import Text.PrettyPrint.Mainland.Class
import qualified Language.Pads.Source as S
import Data.Data

-- | Errors which can be encountered at runtime when parsing a Pads type
data ErrMsg =
   FoundWhenExpecting String String
 | MissingLiteral String
 | ExtraBeforeLiteral String
 | LineError String
 | Insufficient Int Int
 | RegexMatchFail String
 | TransformToDstFail String String String
 | TransformToSrcFail String String String
 | UnderlyingTypedefFail
 | PredicateFailure
 | ExtraStuffBeforeTy String String
 | FileError String String
 | BitWidthError Int Int
   deriving (Typeable, Data, Eq, Ord, Show)

{- XXX-KSF: fix pretty printing to use pretty printing combinators rather than string ++ -}
-- | Pretty printer for Pads runtime error messages.
instance Pretty ErrMsg where
  ppr (FoundWhenExpecting str1 str2) = text ("Encountered " ++ str1 ++ " when expecting " ++ str2 ++ ".")
  ppr (MissingLiteral s)     = text ("Missing Literal: " ++ s ++ ".")
  ppr (ExtraBeforeLiteral s) = text ("Extra bytes before literal: " ++ s ++ ".")
  ppr (ExtraStuffBeforeTy junk ty) = text ("Extra bytes: " ++ junk ++ " before " ++ ty ++ ".")
  ppr (Insufficient found expected) = text("Found " ++ show found ++ " bytes when looking for " ++ show expected ++ "bytes.")
  ppr (RegexMatchFail s) = text ("Failed to match regular expression: " ++ s ++ ".")
  ppr (TransformToDstFail s1 s2 s3) = text ("Parsing transform " ++ s1 ++ " failed on input: " ++ s2 ++ s3)
  ppr (TransformToSrcFail s1 s2 s3) = text ("Printing transform "++ s1 ++ " failed on input: " ++ s2 ++ s3)
  ppr (LineError s)        = text s
  ppr UnderlyingTypedefFail  = text "Pads predicate is true, but underlying type had an error."
  ppr PredicateFailure       = text "Pads predicate is false."
  ppr (FileError err file) = text ("Problem with file: " ++ file ++ "("++ err ++ ").")
  ppr (BitWidthError x y) = text ("Bad field width: " ++ show y ++ " cannot fit in " ++ show x ++ ".")

-- | Error information relating back to the source input
data ErrInfo = ErrInfo { msg      :: ErrMsg,
                         position :: Maybe S.Span }
   deriving (Typeable, Data, Eq, Ord, Show)

-- | Pretty printer for reporting where in the source text a parse error
-- occured.
instance Pretty ErrInfo where
  ppr ErrInfo{msg,position} = ppr msg <+>
       case position of
         Nothing -> empty
         Just pos -> text "at:" <+>  ppr pos

-- | Always just pick the first error message
mergeErrInfo ErrInfo{msg=msg1, position=position1} ErrInfo{msg=msg2, position=position2} =
             ErrInfo{msg=msg1, position=position1}

-- | Merge errors in the Maybe monad
maybeMergeErrInfo m1 m2 = case (m1,m2) of
          (Nothing,Nothing) -> Nothing
          (Just p, Nothing) -> Just p
          (Nothing, Just p) -> Just p
          (Just p1, Just p2) -> Just (mergeErrInfo p1 p2)

