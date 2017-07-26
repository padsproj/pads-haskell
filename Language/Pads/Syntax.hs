{-# LANGUAGE DeriveDataTypeable,DeriveLift, DeriveAnyClass, DeriveGeneric #-}

{-
** *********************************************************************
*     (c) 2011                                                         *
*         Kathleen Fisher <kathleen.fisher@gmail.com>                  *
*         John Launchbury <john.launchbury@gmail.com>                  *
*                                                                      *
************************************************************************
-}



module Language.Pads.Syntax where

import Data.Generics (Data(..), Typeable(..))
import Language.Haskell.TH
import Language.Haskell.TH.Lift (Lift(..))
import GHC.Generics (Generic(..))

instance Lift Pat
instance Lift Exp

data PadsDecl = PadsDeclType   String [String] (Maybe Pat) PadsTy
              | PadsDeclData   String [String] (Maybe Pat) PadsData [QString]
              | PadsDeclNew    String [String] (Maybe Pat) BranchInfo [QString]
              | PadsDeclObtain String [String] PadsTy Exp
   deriving (Eq, Data, Typeable, Show, Lift, Generic)


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
   deriving (Eq, Data, Typeable, Show, Lift, Generic)

data TermCond = LTerm PadsTy | LLen Exp
  deriving (Eq, Data, Typeable, Show, Lift, Generic)


data PadsData = PUnion [BranchInfo]
              | PSwitch Exp [(Pat,BranchInfo)]
  deriving (Eq, Data, Typeable, Show, Lift, Generic)

data BranchInfo = BRecord String [FieldInfo] (Maybe Exp)
                | BConstr String [ConstrArg] (Maybe Exp)
  deriving (Eq, Data, Typeable, Show, Lift, Generic)

type FieldInfo = (Maybe String, ConstrArg, Maybe Exp)
type ConstrArg = (PadsStrict, PadsTy)

data PadsStrict = IsStrict | NotStrict | Unpacked
  deriving (Eq, Data, Typeable, Show, Lift, Generic)

type QString = [String]  -- qualified names


hasRep :: PadsTy -> Bool
hasRep (PExpression l)   = False
hasRep (PTycon ["EOF"])  = False
hasRep (PTycon ["EOR"])  = False
hasRep (PTycon ["Void"]) = False
hasRep ty                 = True

qName :: QString -> String
qName [n] = n
qName (n:ms) = n ++ "." ++ qName ms



