{-# LANGUAGE  TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses
            , FlexibleInstances, DeriveDataTypeable, NamedFieldPuns, ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK prune #-}
{-|
  Module      : Language.Pads.GenPretty
  Description : Template haskell based pretty printing instances
  Copyright   : (c) 2011
                Kathleen Fisher <kathleen.fisher@gmail.com>
                John Launchbury <john.launchbury@gmail.com>
  License     : MIT
  Maintainer  : Karl Cronburg <karl@cs.tufts.edu>
  Stability   : experimental

-}
module Language.Pads.GenPretty where

import Language.Pads.Padsc
import Language.Pads.Errors
import Language.Pads.MetaData
import Language.Pads.TH

import Language.Haskell.TH as TH hiding (ppr)

import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import qualified Data.List as L
import qualified Data.Set as S
import Control.Monad

import System.Posix.Types
import Data.Word
import Data.Int
import Data.Time
import Debug.Trace as D

pprE argE = AppE (VarE 'ppr) argE
pprListEs argEs = ListE (map pprE argEs)
pprCon1E argE = AppE (VarE 'pprCon1) argE
pprCon2E argE = AppE (VarE 'pprCon2) argE

pprCon1 arg = ppr (toList1 arg)
pprCon2 arg = ppr (toList2 arg)

-- | Get all the names of types referenced within the given 'TH.Type'
getTyNames :: TH.Type ->  S.Set TH.Name
getTyNames ty  = case ty of
    ForallT tvb cxt ty' -> getTyNames ty'
    VarT name           -> S.empty
    ConT name           -> S.singleton name
    TupleT i            -> S.empty
    ArrowT              -> S.empty
    ListT               -> S.empty
    AppT t1 t2          -> getTyNames t1 `S.union` getTyNames t2
    SigT ty kind        -> getTyNames ty

-- | Get all the types referenced within the given Haskell constructor.
getTyNamesFromCon :: TH.Con -> S.Set TH.Name
getTyNamesFromCon con = case con of
  (NormalC name stys)   -> S.unions (map (\(_,ty)   -> getTyNames ty) stys)
  (RecC name vstys)     -> S.unions (map (\(_,_,ty) -> getTyNames ty) vstys)
  (InfixC st1 name st2) -> getTyNames (snd st1) `S.union` getTyNames (snd st2)
  (ForallC tvb cxt con) -> getTyNamesFromCon con

-- | Recursively reify types to get all the named types referenced by the given
-- name
getNamedTys :: TH.Name -> Q [TH.Name]
getNamedTys ty_name = S.toList <$> getNamedTys' S.empty (S.singleton ty_name)

-- | Helper for 'getNamedTys'
getNamedTys' :: S.Set TH.Name -> S.Set TH.Name -> Q (S.Set TH.Name)
getNamedTys' answers worklist =
 if S.null worklist then return answers
 else do
   { let (ty_name, worklist') = S.deleteFindMin worklist
   ; let answers' = S.insert ty_name answers
   ; info <- reify ty_name
   ; case info of
        TyConI (NewtypeD [] ty_name' [] _ con derives) -> do
           { let all_nested = getTyNamesFromCon con
           ; let new_nested = all_nested `S.difference` answers'
           ; let new_worklist = worklist' `S.union` new_nested
           ; getNamedTys' answers' new_worklist
           }
        TyConI (DataD [] ty_name' [] _ cons derives) -> do
           { let all_nested = S.unions (map getTyNamesFromCon cons)
           ; let new_nested = all_nested `S.difference` answers'
           ; let new_worklist = worklist' `S.union` new_nested
           ; getNamedTys' answers' new_worklist
           }
        TyConI (TySynD _ _ _ ) -> do {reportError ("getTyNames: unimplemented TySynD case " ++ (nameBase ty_name)); return answers'}
        TyConI (ForeignD _) -> do {reportError ("getTyNames: unimplemented ForeignD case " ++ (nameBase ty_name)); return answers'}
        PrimTyConI _ _ _ -> return answers
        otherwise -> do {reportError ("getTyNames: pattern didn't match for " ++ (nameBase ty_name)); return answers'}
   }

-- | All the base types supported by Pads
baseTypeNames = S.fromList [ ''Int, ''Char, ''Digit, ''Text, ''String, ''StringFW, ''StringME
                           , ''StringSE, ''COff, ''EpochTime, ''FileMode, ''Int, ''Word, ''Int64
                           , ''Language.Pads.Errors.ErrInfo, ''Bool, ''Binary, ''Base_md, ''UTCTime, ''TimeZone
                           ]

-- | Recursively make the pretty printing instance for a given named type by
-- also making instances for all nested types.
mkPrettyInstance :: TH.Name -> Q [TH.Dec]
mkPrettyInstance ty_name = mkPrettyInstance' (S.singleton ty_name) baseTypeNames []

mkMe :: TH.Name -> Q [TH.Dec]
mkMe n = do
  D.traceM "HELLOOOOOOOOOO"
  return []

-- | Helper for 'mkPrettyInstance'
mkPrettyInstance' :: S.Set TH.Name -> S.Set TH.Name -> [TH.Dec] -> Q [TH.Dec]
mkPrettyInstance' worklist done decls =
  if S.null worklist then return decls
  else do
      let (ty_name, worklist') = S.deleteFindMin worklist
      if ty_name `S.member` done then mkPrettyInstance' worklist' done decls
         else do
         let tyBaseName = nameBase ty_name
         let baseStr = strToLower tyBaseName
         let specificPprName = mkName (baseStr ++ "_ppr")
         let funName = mkName (strToLower (tyBaseName ++ "_ppr"))
         let inst = AppT (ConT ''Pretty) (ConT ty_name)
         let genericPprName = mkName "ppr"
         let ppr_method = ValD (VarP genericPprName) (NormalB (VarE specificPprName)) []
         let instD = InstanceD Nothing [] inst [ppr_method]
         let newDone = S.insert ty_name done
         info <- reify ty_name
         (nestedTyNames, decls') <- case info of
                   TyConI (NewtypeD [] ty_name' [] _ (NormalC ty_name'' [(Bang NoSourceUnpackedness NoSourceStrictness, AppT ListT ty)]) derives) -> do -- List
                     { let nestedTyNames = getTyNames ty
--                     ; reportError ("list case " ++ (nameBase ty_name))
                     ; (itemsE,itemsP) <- doGenPE "list"
                     ; let mapE  = AppE (AppE (VarE 'map) (VarE 'ppr)) itemsE
                     ; let bodyE = AppE (AppE (VarE 'namedlist_ppr) (nameToStrLit ty_name)) mapE
                     ; let argP = ConP (mkName tyBaseName) [itemsP]
                     ; let clause = Clause [argP] (NormalB bodyE) []
                     ; return (nestedTyNames, [instD, FunD specificPprName [clause]])
                     }
                   TyConI (NewtypeD [] ty_name' [] _ (NormalC ty_name'' [(Bang NoSourceUnpackedness NoSourceStrictness, AppT (AppT (ConT ty_con_name) ty_arg1) ty_arg2) ]) derives) -> do  -- curry rep (Map)
                     { let nestedTyNames = getTyNames ty_arg2
                     ; (argP, body) <- mkPatBody tyBaseName pprCon2E
--                     ; reportError ("curry rep case " ++ (nameBase ty_name))
                     ; let clause = Clause [argP] body []
                     ; return (nestedTyNames, [instD, FunD specificPprName [clause]])
                     }
                   TyConI (NewtypeD [] ty_name' [] _ (NormalC ty_name'' [(Bang NoSourceUnpackedness NoSourceStrictness, AppT (ConT ty_con_name) ty_arg) ]) derives) -> do  -- con rep (Set)
                     { let nestedTyNames = getTyNames ty_arg
                     ; (argP, body) <- mkPatBody tyBaseName pprCon1E
--                     ; reportError ("con rep case " ++ (nameBase ty_name))
                     ; let clause = Clause [argP] body []
                     ; return (nestedTyNames, [instD, FunD specificPprName [clause]])
                     }
                   TyConI (NewtypeD [] ty_name' [] _ (NormalC ty_name'' [(Bang NoSourceUnpackedness NoSourceStrictness, ConT core_name)]) derives) -> do  -- App, Typedef
                     { (argP, body) <- mkPatBody tyBaseName pprE
--                     ; reportError ("app, typedef case " ++ (nameBase ty_name))
                     ; let clause = Clause [argP] body []
                     ; return (S.singleton core_name, [instD, FunD specificPprName [clause]])
                     }
                   TyConI (NewtypeD [] ty_name' [] _ (NormalC ty_name'' [(Bang NoSourceUnpackedness NoSourceStrictness, ty)]) derives) | isTuple ty -> do    -- Tuple
                     { let nestedTyNames = getTyNames ty
--                     ; reportError ("tuple case " ++ (nameBase ty_name))
                     ; let (len, tys) = tupleTyToListofTys ty
                     ; (exps, pats) <- doGenPEs len "tuple"
                     ; let bodyE = AppE (AppE (VarE 'namedtuple_ppr) (LitE (StringL tyBaseName)))  (pprListEs exps)
                     ; let argP = ConP (mkName tyBaseName) [TupP pats]
                     ; let clause = Clause [argP] (NormalB bodyE) []
                     ; return (nestedTyNames, [instD, FunD specificPprName [clause]])
                     }
                   TyConI (DataD [] ty_name' [] _ cons  derives) | isDataType cons -> do
                     { let nestedTyNames = S.unions (map getTyNamesFromCon cons)
                     ; (exp, pat) <- doGenPE "case_arg"
                     ; matches <- mapM mkClause cons
                     ; let caseE = CaseE exp matches
                     ; let clause = Clause [pat] (NormalB caseE) []
                     ; return (nestedTyNames, [instD, FunD specificPprName [clause]] )
                     }
                   TyConI (DataD [] ty_name' [] _ cons  derives) | isRecordType cons -> do
                     { let nestedTyNames = S.unions (map getTyNamesFromCon cons)
--                   ; report (length cons /= 1) ("GenPretty: record " ++ (nameBase ty_name')  ++ " did not have a single constructor.")
                     ; clause <- mkRecord (L.head cons)
                     ; return (nestedTyNames, [instD, FunD specificPprName [clause]])
                     }
                   TyConI (DataD _ ty_name' _ _ cons  derives) -> do
                    {
--                      reportError ("DataD pattern didn't match for"++(nameBase ty_name))
                    ; return (S.empty, [])}
                   TyConI (TySynD ty_name' [] ty) -> do
                     { let nestedTyNames = getTyNames ty
--                     ; reportError ("tysyn for"++(nameBase ty_name))
                     ; return (nestedTyNames, [])}
                   TyConI (TySynD ty_name' tyVarBndrs ty) -> do
                     { let nestedTyNames = getTyNames ty
--                     ; reportError ("tysyn for"++(nameBase ty_name))
                     ; return (nestedTyNames, [])}
                   TyConI dec -> do {reportError ("otherwise; tyconI case "++(nameBase ty_name)) ; return (S.empty, [])}
                   otherwise -> do {reportError ("pattern didn't match for "++(nameBase ty_name)) ; return (S.empty, [])}
         let newWorklist = worklist `S.union` nestedTyNames
         let newDecls = decls'++decls
         mkPrettyInstance' newWorklist newDone newDecls

-- | Is the given type a TupleT?
isTuple (TupleT n) = True
isTuple (AppT ty _) = isTuple ty

-- | Is the given constructor a normal Haskell constructor?
isDataType [] = False
isDataType (NormalC _ _ : rest) = True
isDataType _ = False

-- | Is the given constructor a Haskell record constructor?
isRecordType [] = False
isRecordType (RecC _ _ : rest) = True
isRecordType _ = False

-- | Make the pattern body of a pretty printer expression for a named Pads type
mkPatBody core_name_str pprE = do
  (exp,pat) <- doGenPE "arg"
  bodyE <- [| namedty_ppr $(litE $ stringL core_name_str) $(return $ pprE exp) |]
  argP  <- conP (mkName core_name_str) [return pat]
  return (argP, NormalB bodyE)

-- | Make the pattern body of a pretty printer expression for a Pads type /
-- data constructor without arguments.
mkPatBodyNoArg core_name_str = do
  bodyE <- [| text $(litE $ stringL core_name_str) |]
  argP <- conP (mkName core_name_str) []
  return (argP, NormalB bodyE)

-- | Make the clause for the data constructor of a data type based on whether or
-- not it has any arguments.
mkClause con = case con of
     NormalC name [] -> do
        { (argP, body) <- mkPatBodyNoArg (nameBase name)
        ; return (Match argP body [])
        }
     NormalC name ty_args -> do
        { (argP, body) <- mkPatBody (nameBase name) pprE
        ; return (Match argP body [])
        }
     otherwise -> error "mkClause not implemented for this kind of constructor."

-- | Make the Haskell clause for a Pads record.
mkRecord (RecC rec_name fields) = do
  fieldInfo <- mapM mkField fields
  let (recPs, recEs) = unzip fieldInfo
  let recP = RecP rec_name recPs
  let bodyE = AppE (AppE (VarE 'record_ppr) (nameToStrLit rec_name)) (ListE recEs)
  return (Clause [recP] (NormalB bodyE) [])

-- | Make the field pretty printer case.
mkField (field_name, _, ty) = do
  (expE, pat) <- doGenPE (nameBase field_name)
  fieldE <- [| field_ppr $(return $ nameToStrLit field_name) $(return $ pprE expE) |]
  return ((field_name, pat), fieldE)

nameToStrLit name = LitE (StringL (nameBase name))

-- instance Pretty a => Pretty (PMaybe a) where
--   ppr PNothing = text ""
--   ppr (PJust a) = ppr a
--
-- instance Pretty a => Pretty (PMaybe_imd a) where
--   ppr (PNothing_imd _) = text ""
--   ppr (PJust_imd a) = ppr a
