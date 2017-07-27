{-# LANGUAGE DeriveDataTypeable,DeriveLift, DeriveAnyClass, DeriveGeneric #-}
{-|
  Module      : Language.Pads.Syntax
  Description : Short description
  Copyright   : (c) 2011
                Kathleen Fisher <kathleen.fisher@gmail.com>
                John Launchbury <john.launchbury@gmail.com>
  License     : BSD3
  Maintainer  : Karl Cronburg <karl@cs.tufts.edu>
  Stability   : experimental

  Haskell data types and instances for the syntax of Pads.

-}
module Language.Pads.Syntax where

import Data.Generics (Data(..), Typeable(..))
import Language.Haskell.TH
import Language.Haskell.TH.Lift (Lift(..))
import GHC.Generics (Generic(..))

instance Lift Pat
instance Lift Exp

-- | AST form of a pads declaration with four flavors.
data PadsDecl
    -- | A pads type declaration e.g.:
    --
    -- > [pads| type           Foo     x y z   (foo :: Int) = (x, y, z, foo) |]
             =  PadsDeclType   String [String] (Maybe Pat) PadsTy

    -- | A pads data declaration e.g.:
    --
    -- > [pads| data           Foo     x y z   (foo :: Int) = Foo (x, y, z, foo) deriving (Eq, Ord, Show) |]
             |  PadsDeclData   String [String] (Maybe Pat)    PadsData                    [QString]

    -- | A pads newtype declaration e.g.:
    --
    -- > [pads| newtype        Foo     x y z   (foo :: Int) = Foo (x, y, z, foo) deriving (Eq, Ord, Show) |]
             |  PadsDeclNew    String [String] (Maybe Pat)    BranchInfo                  [QString]

    -- | A pads declaration for obtaining one type after parsing it from another, e.g.:
    --
    -- > [pads| obtain         Foo     x y z   from   Int    using <|(fncn,inverse)|> |]
             |  PadsDeclObtain String [String]        PadsTy          Exp
   deriving (Eq, Data, Typeable, Show, Lift, Generic)

-- | AST form of a pads type, as notably used to the right hand side of an
-- equals sign in a @'PadsDecl'@ pads declaration.
data PadsTy
  -- | AST form of "constrain @'Pat'@ :: @'PadsTy'@ where @'Exp'@" e.g.:
  --
  -- > [pads| constrain x :: Digit where <|x `mod` 2|> == 0|> |]
    = PConstrain Pat PadsTy Exp

  -- | AST form of "transform @'PadsTy'@ => @'PadsTy'@ using @'Exp'@" e.g.:
  --
  -- > [pads| transform StringFW 1 => Char using <|(head, list1)|> |]
    | PTransform PadsTy PadsTy Exp

  -- | AST form of a list of some @'PadsTy'@ type, comes with two optional attributes e.g.:
  -- "[ @'PadsTy'@ | @'PadsTy'@ ] terminator @'TermCond'@"
  --
  -- The following @'PadsTy'@ describes a comma-separated list of integers
  -- terminated by the EOF symbol:
  --
  -- > [pads| [Int | ','] terminator EOF |]
    | PList PadsTy (Maybe PadsTy) (Maybe TermCond)

  -- | AST form of a partitioned type "partition @'PadsTy'@ using @'Exp'@" e.g.:
  --
  -- > [pads| partition Entries using <| bytes 6 |> |]
  --
  -- A partitioned type allows for parser extensions to make use of the state of
  -- the PADS parser in deciding how to divide up (partition) the input.
    | PPartition PadsTy Exp

  -- | AST form of a value constructor "value @'Exp'@ :: @'PadsTy'@" e.g.:
  --
  -- > [pads| data Foo = Foo { x :: Int, xIsEven = value <| even x |> :: Bool } |]
  --
  -- This allows you to do the opposite of what @'BConstr'@ does: bring names into
  -- scope which get stored in the output of the parser (rather than having them
  -- disappear after the parser finishes.
    | PValue Exp PadsTy

  -- | A pads type application like "@'PadsTy'@ @'PadsTy'@ @'PadsTy'@ ... @'Exp'@" e.g.
  --
  -- > [pads| data Foo      = Foo { x :: Int, Bar x <| x + 1 |> |]
    | PApp [PadsTy] (Maybe Exp)

  -- | AST form of a pads tuple "( @'PadsTy@', @'PadsTy@', ... )" e.g.
  --
  -- > [pads| (Int, "+", Int) |]
    | PTuple [PadsTy]

  -- | An arbitrary Haskell expression as used in a @'PApp'@ pads type application
  -- and in a @'PSwitch'@ pads switch/case type.
    | PExpression Exp

  -- | Pads type constructor with a qualified name
    | PTycon QString

  -- | Pads type variable with a name
    | PTyvar String
   deriving (Eq, Data, Typeable, Show, Lift, Generic)

-- | Parser terminator condition
data TermCond
  -- | Lexical terminator type: any @'PadsTy'@ signaling termination
    = LTerm PadsTy

  -- | Lexical length: arbitrary Haskell @'Exp@'
    | LLen Exp
  deriving (Eq, Data, Typeable, Show, Lift, Generic)

-- | Right-hand side of a pads data type declaration
data PadsData
  -- | A pads union data type declaration.
  -- Syntax: "@'BranchInfo'@ | @'BranchInfo'@ | ..."
    = PUnion [BranchInfo]

  -- | A pads switch-case 'statement'.
  --
  -- Syntax:
  --
  -- @
  --    case 'Exp' of
  --      'Pat' -> 'BranchInfo'
  --    | 'Pat' -> 'BranchInfo'
  --    ...
  -- @
  --
  -- > [pads| case <| tag + 1 |> of
  -- >          2 -> Foo
  -- >        | 3 -> Bar
  -- > |]
    | PSwitch Exp [(Pat,BranchInfo)]
  deriving (Eq, Data, Typeable, Show, Lift, Generic)

-- | An individual branch of some pads data type, either defining a Haskell record
-- parser or a Haskell constructor parser.
data BranchInfo
  -- | Branch record with a constructor name, list of record fields, and maybe a boolean 'where' clause.
  --
  -- Syntax: @'String'@ { @'FieldInfo'@, @'FieldInfo'@, ... } where @'Exp@'
  --
  -- > [pads| Foo { x :: Int, y :: Char } where <| x == ord y |>
    = BRecord String [FieldInfo] (Maybe Exp)

  -- | Branch constructor with a constructor name, a list of argument types, and maybe a boolean 'where' clause:
  --
  -- Syntax: @'String'@ @'ConstrArg'@ @'ConstrArg'@ ... where @'Exp@'
  --
  -- > [pads| Foo (x :: Int) (y :: Char) where <| x == ord y |>
  --
  -- Note that this lets you bring variables into scope during parsing (`x` and
  -- `y` in the above) *without* saving them into the parse result, effectively
  -- making them operate as temporary variables that can be referenced by the
  -- Haskell predicates.
    | BConstr String [ConstrArg] (Maybe Exp)
  deriving (Eq, Data, Typeable, Show, Lift, Generic)

-- | Individual field of a pads record, "@'String'@ :: @'ConstrArg'@ where @'Exp'@"
type FieldInfo = (Maybe String, ConstrArg, Maybe Exp)
type ConstrArg = (PadsStrict, PadsTy)

-- | A hold-over resulting from a deprecation moving from an older version of template-haskell.
data PadsStrict = IsStrict | NotStrict | Unpacked
  deriving (Eq, Data, Typeable, Show, Lift, Generic)

-- | Qualified names where ["Foo", "Bar"] means "Foo.Bar"
type QString = [String]

-- | Whether or not a @'PadsTy'@ has an underlying Haskell representation
hasRep :: PadsTy -> Bool
hasRep (PExpression l)   = False
hasRep (PTycon ["EOF"])  = False
hasRep (PTycon ["EOR"])  = False
hasRep (PTycon ["Void"]) = False
hasRep ty                 = True

-- | > ["Foo", "Bar"] -> "Foo.Bar"
qName :: QString -> String
qName [n] = n
qName (n:ms) = n ++ "." ++ qName ms



