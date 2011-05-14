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



data PadsDecl = PadsDeclType  Bool UString [LString] (Maybe Pat) PadsTy
              | PadsDeclData  Bool UString [LString] (Maybe Pat) PadsData [UString]
              | PadsDeclNew   Bool UString [LString] (Maybe Pat) BranchInfo [UString]
   deriving (Eq, Data, Typeable, Show)


data PadsTy = PConstrain Pat PadsTy Exp
            | PTransform PadsTy PadsTy Exp
            | PList PadsTy (Maybe PadsTy) (Maybe TermCond)
            | PPartition PadsTy Exp
            | PApp [PadsTy] (Maybe Exp)
            | PTuple [PadsTy] 
            | PExpression Exp
            | PTycon UString
            | PTyvar LString
   deriving (Eq, Data, Typeable, Show)

data TermCond = LTerm PadsTy | LLen Exp
  deriving (Eq, Data, Typeable, Show)


data PadsData = PUnion [BranchInfo]
              | PSwitch Exp [(Pat,BranchInfo)]
  deriving (Eq, Data, Typeable, Show)

data BranchInfo = BRecord UString [FieldInfo] (Maybe Exp)
                | BConstr UString [ConstrArg] (Maybe Exp)
  deriving (Eq, Data, Typeable, Show)

type FieldInfo = (Maybe LString, ConstrArg, Maybe Exp)
type ConstrArg = (Strict, PadsTy)

type UString = String
type LString = String


hasRep :: PadsTy -> Bool
hasRep (PExpression l) = False
hasRep (PTycon "EOF")  = False
hasRep (PTycon "EOR")  = False
hasRep (PTycon "Void") = False
hasRep _               = True

