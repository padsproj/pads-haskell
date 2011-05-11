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



data PadsDecl = PadsDeclType UString [LString] (Maybe Pat) PadsTy
              | PadsDeclData UString [LString] (Maybe Pat) PadsData [UString]
              | PadsDeclNew  UString [LString] (Maybe Pat) BranchInfo [UString]
              | PadsDeclOld  UString [LString] (Maybe Pat) PadsTy
   deriving (Eq, Data, Typeable, Show)


data PadsTy = PConstrain Pat PadsTy Exp
            | PTransform PadsTy PadsTy Exp
            | PList PadsTy (Maybe PadsTy) (Maybe TermCond)
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
hasRep _               = True

