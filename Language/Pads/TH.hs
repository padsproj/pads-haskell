{-# LANGUAGE TemplateHaskell #-}


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
import Data.Data
import Control.Monad
import Char

typeAnnotate :: Maybe TH.Pat -> Maybe (TH.Pat, TH.Type)
typeAnnotate Nothing = Nothing
typeAnnotate (Just pat) = Just (pat, patToTy pat)

mergeMaybe m1 m2 = case (m1,m2) of
  (Nothing, Nothing) -> Nothing
  (Just d1, Just d2) -> Just (d1,d2)
  _ -> error "mergeMaybe given two maybes in different states."


flattenMaybeList xs = case xs of
  [] -> []
  (Nothing: xxs) -> flattenMaybeList xxs
  (Just x : xxs) -> x : flattenMaybeList xxs

mapFstChar f [] = []
mapFstChar f (c:cs) = (f c) : cs

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

arrowTy ty1 ty2 = (ArrowT `AppT` ty1) `AppT` ty2


mkListT, mkTupleT :: [Type] -> Type
mkListT ts = foldl AppT ListT ts
mkTupleT ts = foldl AppT (TupleT (length ts)) ts

listTH, tupleTH :: [Name] -> Q Exp
listTH xs = return (ListE (map VarE xs))
tupleTH xs = return (TupE (map VarE xs))



tupleTyToListofTys (AppT (TupleT n) ty) = (n, collect ty)
  where collect (AppT ty' tys') = ty' : (collect tys')
        collect ty = [ty]

tupleTyToListofTys ty = collect ty []
  where collect (TupleT n) acc = (n, acc)
        collect (AppT tys ty) acc = collect tys (ty:acc)

genPE name = (VarE name, VarP name)

genPEQ :: Name -> (Q Exp, Q Pat)
genPEQ name = (return (VarE name), return (VarP name))

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
  InfixP p1 n p2 -> error ("Infix constructor "++ (showName n) ++ " application needs a type annotation.")
  TildeP p    -> patToTy p
  BangP  p    -> patToTy p
  AsP n p     -> patToTy p
  WildP       -> error "Wild card patterns are not supported in PADS declarations."
  RecP name fieldPats -> ConT name   {-  I think this is the correct represtentation of a line type. -}
  ListP pats  -> mkListT (map patToTy pats)
  SigP p ty   -> ty
  
litToTy :: TH.Lit -> TH.Type
litToTy lit = 
  let name = case lit of
       CharL c       -> ''Char
       StringL s     -> ''String
       IntegerL i    -> ''Integer
       RationalL r   -> ''Rational
       IntPrimL  i   -> ''Integer
       WordPrimL i   -> ''Integer
       FloatPrimL f  -> ''Rational
       DoublePrimL d -> ''Rational
  in ConT name

patToExp :: TH.Pat -> TH.Exp
patToExp pat = case pat of
  LitP l      -> LitE l
  VarP n      -> VarE n
  TupP pats   -> TupE (map patToExp pats)
  InfixP p1 n p2 -> InfixE (Just (patToExp p1)) (VarE n) (Just (patToExp p2))
  TildeP p    -> patToExp p
  BangP  p    -> patToExp p
  AsP n p     -> VarE n
  WildP       -> error "Wild card patterns are not supported in PADS declarations. Can't convert to expression"
  RecP name fieldPats -> RecConE name (map fieldPatToExp fieldPats)   {-  I think this is the correct represtentation of a line type. -}
  ListP pats  -> ListE (map patToExp pats)
  SigP p ty   -> patToExp p

fieldPatToExp (n,p) = (n, patToExp p)

boolToExpE True = ConE 'True
boolToExpE False = ConE 'False
