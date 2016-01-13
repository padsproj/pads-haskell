{-# LANGUAGE TemplateHaskell, MagicHash #-}
{-# OPTIONS_GHC -Wall #-}

{-
** *********************************************************************
*                                                                      *
*         (c)  Kathleen Fisher <kathleen.fisher@gmail.com>             *
*              John Launchbury <john.launchbury@gmail.com>             *
*                                                                      *
************************************************************************
-}


module Language.Pads.TH where

import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Control.Monad
import Data.Char
import GHC.Exts

typeAnnotate :: Maybe TH.Pat -> Maybe (TH.Pat, TH.Type)
typeAnnotate Nothing = Nothing
typeAnnotate (Just pat) = Just (pat, patToTy pat)

mergeMaybe :: Maybe a -> Maybe b -> Maybe (a,b)
mergeMaybe m1 m2 = case (m1,m2) of
  (Nothing, Nothing) -> Nothing
  (Just d1, Just d2) -> Just (d1,d2)
  _ -> error "mergeMaybe given two maybes in different states."

mapFstChar :: (Char -> Char) -> String -> String
mapFstChar _ [] = []
mapFstChar f (c:cs) = f c : cs

strToUpper, strToLower :: String -> String
strToUpper = mapFstChar toUpper
strToLower = mapFstChar toLower

applyE :: [Exp] -> Exp
applyE ts = foldl1 AppE ts

applyT :: [Type] -> Type
applyT ts = foldl1 AppT ts

abstractT :: Type -> [Type] -> Type
abstractT t ts = foldr arrow t ts
  where
    t1 `arrow` t2 = (ArrowT `AppT` t1) `AppT` t2

arrowTy :: Type -> Type -> Type
arrowTy ty1 ty2 = (ArrowT `AppT` ty1) `AppT` ty2


mkListT, mkTupleT :: [Type] -> Type
mkListT ts = foldl AppT ListT ts
mkTupleT ts = foldl AppT (TupleT (length ts)) ts

listTH, tupleTH :: [Name] -> Q Exp
listTH xs = return (ListE (map VarE xs))
tupleTH xs = return (TupE (map VarE xs))



tupleTyToListofTys :: Type -> (Int,[Type])

-- for tuples encoded as AppT (,) (AppT a b)
tupleTyToListofTys (AppT (TupleT n) ty0) = (n, collect ty0)
  where
  collect ty = case ty of
    AppT ty' tys' -> ty' : collect tys'
    _             -> [ty]

-- for tuples encoded as AppT (AppT (,) a) b
tupleTyToListofTys ty0 = collect ty0 []
  where
  collect ty acc = case ty of
    TupleT n     -> (n, acc)
    AppT tys ty' -> collect tys (ty':acc)
    _            -> error "typeleTyToListofTys: unexpected type"

genPE :: Name -> (Exp,Pat)
genPE name = (VarE name, VarP name)

genPEQ :: Name -> (Q Exp, Q Pat)
genPEQ name = (return (VarE name), return (VarP name))

doGenPE :: String -> Q (Exp,Pat)
doGenPE str = do {
  ; name <- newName str
  ; return (VarE name, VarP name)
  }

doGenPEs :: Int -> String -> Q([TH.Exp], [TH.Pat])
doGenPEs n str = do 
  { varpats <- replicateM n (doGenPE str)
  ; return (unzip varpats)
  }

{- XXX: need to add location information so can report location of error messages. -}
patToTy :: TH.Pat -> TH.Type
patToTy pat = case pat of
  LitP l      -> litToTy l
  VarP n      -> error ("Variable "++ (showName n) ++ " needs a type annotation.")
  TupP pats   -> mkTupleT (map patToTy pats)
  InfixP _ n _ -> error ("Infix constructor "++ (showName n) ++ " application needs a type annotation.")
  TildeP p    -> patToTy p
  BangP  p    -> patToTy p
  AsP _ p     -> patToTy p
  WildP       -> error "Wild card patterns are not supported in PADS declarations."
  {- I think this is the correct represtentation of a line type. -}
  RecP name _fieldPats -> ConT name
  ListP pats  -> mkListT (map patToTy pats)
  SigP _ ty   -> ty
  ParensP pat' -> patToTy pat'
  _           -> error ("patToTy: unexpected pat: " ++ show pat)

litToTy :: TH.Lit -> TH.Type
litToTy lit = ConT $ case lit of
  CharL _       -> ''Char
  StringL _     -> ''String
  IntegerL _    -> ''Integer
  RationalL _   -> ''Rational
  IntPrimL  _   -> ''Integer
  WordPrimL _   -> ''Integer
  FloatPrimL _  -> ''Rational
  DoublePrimL _ -> ''Rational
  _             -> error ("litToTy: unexpected lit: " ++ show lit)

patToExp :: TH.Pat -> TH.Exp
patToExp pat = case pat of
  LitP l      -> LitE l
  VarP n      -> VarE n
  TupP pats   -> TupE (map patToExp pats)
  InfixP p1 n p2 -> InfixE (Just (patToExp p1)) (VarE n) (Just (patToExp p2))
  TildeP p    -> patToExp p
  BangP  p    -> patToExp p
  AsP n _     -> VarE n
  WildP       -> error "Wild card patterns are not supported in PADS \
                       \ declarations. Can't convert to expression"

  {-  I think this is the correct represtentation of a line type. -}
  RecP name fieldPats -> RecConE name (map fieldPatToExp fieldPats)
  ListP pats  -> ListE (map patToExp pats)
  SigP p _ty  -> patToExp p
  ParensP p   -> patToExp p
  _           -> error ("patToExp: unexpected pat: " ++ show pat)

fieldPatToExp :: (a,Pat) -> (a,Exp)
fieldPatToExp (n,p) = (n, patToExp p)

boolToExpE :: Bool -> Exp
boolToExpE True = ConE 'True
boolToExpE False = ConE 'False

-- generate globally scoped unique variables as suggested in https://ghc.haskell.org/trac/ghc/ticket/5398
mangleName :: Name -> Name
mangleName name@(Name occ fl) = case fl of
  NameU (I# u) -> Name (mangle_occ occ u) fl
  _            -> name
  where
  mangle_occ :: OccName -> Int# -> OccName
  mangle_occ occ' uniq = mkOccName (occString occ' ++ "_" ++ show (I# uniq))
