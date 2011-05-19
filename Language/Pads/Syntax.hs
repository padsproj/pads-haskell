{-# LANGUAGE DeriveDataTypeable #-}

{-
** *********************************************************************
*     (c) 2011                                                         *
*         Kathleen Fisher <kathleen.fisher@gmail.com>                  *
*         John Launchbury <john.launchbury@gmail.com>                  *
*                                                                      *
************************************************************************
-}



module Language.Pads.Syntax where

import Data.Generics
import Language.Haskell.TH



data PadsDecl = PadsDeclType   String [String] (Maybe Pat) PadsTy
              | PadsDeclData   String [String] (Maybe Pat) PadsData [QString]
              | PadsDeclNew    String [String] (Maybe Pat) BranchInfo [QString]
              | PadsDeclObtain String [String] PadsTy Exp
   deriving (Eq, Data, Typeable, Show)


data PadsTy = PConstrain Pat PadsTy Exp
            | PTransform PadsTy PadsTy Exp
            | PList PadsTy (Maybe PadsTy) (Maybe TermCond)
            | PPartition PadsTy Exp
            | PValue Exp PadsTy
            | PApp [PadsTy] (Maybe Exp)
            | PTuple [PadsTy] 
            | PExpression Exp
            | PTycon QString
            | PTyvar String
   deriving (Eq, Data, Typeable, Show)

data TermCond = LTerm PadsTy | LLen Exp
  deriving (Eq, Data, Typeable, Show)


data PadsData = PUnion [BranchInfo]
              | PSwitch Exp [(Pat,BranchInfo)]
  deriving (Eq, Data, Typeable, Show)

data BranchInfo = BRecord String [FieldInfo] (Maybe Exp)
                | BConstr String [ConstrArg] (Maybe Exp)
  deriving (Eq, Data, Typeable, Show)

type FieldInfo = (Maybe String, ConstrArg, Maybe Exp)
type ConstrArg = (Strict, PadsTy)

type QString = [String]  -- qualified names


hasRep :: PadsTy -> Bool
hasRep (PExpression l)   = False
hasRep (PTycon ["EOF"])  = False
hasRep (PTycon ["EOR"])  = False
hasRep (PTycon ["Void"]) = False
hasRep _                 = True

qName :: QString -> String
qName [n] = n
qName (n:ms) = n ++ "." ++ qName ms
