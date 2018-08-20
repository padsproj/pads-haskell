{-# LANGUAGE NamedFieldPuns,RecordWildCards #-}
{-# OPTIONS_HADDOCK prune #-}
{-|
  Module      : Language.Pads.Pretty
  Description : Pretty printing utilities for Pads types
  Copyright   : (c) 2011
                Kathleen Fisher <kathleen.fisher@gmail.com>
                John Launchbury <john.launchbury@gmail.com>
  License     : MIT
  Maintainer  : Karl Cronburg <karl@cs.tufts.edu>
  Stability   : experimental

-}
module Language.Pads.Pretty where
import Data.Char (isPrint, ord)
import qualified Data.Map as M
import qualified Language.Haskell.TH as TH

import Text.PrettyPrint.Mainland as PP
import Text.PrettyPrint.Mainland.Class
import Language.Pads.Syntax
import qualified Language.Pads.Parser as P
import Language.Pads.RegExp


instance Pretty PadsDecl where
    ppr (PadsDeclType con vars pat padsty _) = text "type" <+> (ppr_decl_lhs con vars pat) <+> text "=" <+> ppr padsty
    ppr (PadsDeclData con vars pat padsrhs cons) = text "data" <+> (ppr_decl_lhs con vars pat) <+> text "="
                                                   <+> ppr padsrhs <> ppr_derives cons
    ppr (PadsDeclNew  con vars pat branchInfo cons) = text "newtype" <+> (ppr_decl_lhs con vars pat) <+> text "="
                                                   <+> ppr branchInfo <> ppr_derives cons

ppr_decl_lhs id args patOpt = text id <> ppr_args args <> ppr_optArgPat patOpt
ppr_derives cons = case cons of
                        []  -> empty
                        [l] -> space <> text (qName l)
                        ls  -> space <> tuple (map (text . qName) cons)

ppr_args args = case args of [] -> empty ; _ -> space <> (spread (map text args))


instance Pretty PadsData where
  ppr (PUnion branches) = ppr_branches branches
  ppr (PSwitch exp patBranches) = text "case" <+> ppr exp <+> text "of" <+> ppr_patBranches patBranches

ppr_patBranches patBranches = enclosesep empty empty (text " |") (map ppr_patBranch patBranches)
ppr_patBranch (pat, branchInfo) = ppr pat <+> text "->" <+> ppr branchInfo
ppr_branches branches = enclosesep empty empty (text " |") (map ppr branches)

instance Pretty BranchInfo where
  ppr (BRecord con fields predOpt)     = text con <+> braces (commasep (map ppr_fieldInfo fields)) <> (ppr_optPred predOpt)
  ppr (BConstr con constrArgs predOpt) = (ppr_conApp con constrArgs) <> (ppr_optPred predOpt)

ppr_conApp con constrArgs = case constrArgs of
  [(NotStrict,arg)] | argIsCon con arg  -> text con
  otherwise -> text con <+> (spread (map ppr_constrArgApp constrArgs))


argIsCon con arg = case arg of
  PExpression (TH.LitE (TH.StringL str)) -> con == str
  otherwise -> False

ppr_fieldInfo (varOpt, constrArg, expOpt, _)
   =  (case varOpt of Nothing -> empty ; Just var ->  (text var) <+> (text "::") <> space)
   <>  ppr_constrArg constrArg
   <>  ppr_optPred expOpt

ppr_constrArg (strict, padsTy)  = ppr_strict strict <> ppr padsTy

ppr_constrArgApp (strict, padsTy)
 | isAtomicTy padsTy = ppr_strict strict <> ppr padsTy
 | otherwise         = ppr_strict strict <> parens (ppr padsTy)

ppr_alpha padsTy
  | isAtomicTy padsTy = ppr padsTy
  | otherwise         = parens (ppr padsTy)

ppr_strict IsStrict  = text "!"
ppr_strict NotStrict = empty

isAtomicTy (PList _ _ _ )   = True
isAtomicTy (PTycon _ )      = True
isAtomicTy (PTyvar _ )      = True
isAtomicTy (PExpression _ ) = True
isAtomicTy (PTuple _ )      = True
isAtomicTy _                = False


instance Pretty PadsTy where
    ppr (PConstrain pat ty exp)  = text "constrain" <+> ppr pat <+> text "::"  <+> ppr ty  <+> text "where" <+> ppr exp
    ppr (PTransform sty dty exp _) = text "transform" <+> ppr sty <+> text "=>" <+> ppr dty <+> text "using" <+> ppr exp
    ppr (PList itemTy sepTy termTy) = ppr_padsList itemTy sepTy termTy
    ppr (PValue exp ty) = text "value" <+> ppr exp <+> text "::"  <+> ppr ty
    ppr (PApp argTys expArgOpt) = spread (map ppr_alpha argTys) <> ppr_opt expArgOpt
    ppr (PTuple tys) = tuple (map ppr tys)
    ppr (PExpression exp) = ppr exp
    ppr (PTycon con) = text (qName con)
    ppr (PTyvar var) = text var

instance Pretty TH.Pat where
  ppr = text . TH.pprint

pprHpat pat = case pat of
  TH.TupP p -> ppr pat
  otherwise -> parens (ppr pat)

instance Pretty TH.Exp where
  ppr =  pprHexp

pprHexp exp = case exp of
  TH.VarE name -> text(TH.pprint exp)
  TH.ConE name -> text(TH.pprint exp)
  TH.LitE lit  -> text(TH.pprint exp)
  TH.AppE (TH.ConE re) (TH.LitE (TH.StringL str)) | re==TH.mkName "RE"
               -> text("'"++str++"'")
  otherwise    -> ppr_bird (text(TH.pprint exp))

ppr_bird s = (text "<|") <> s <> (text "|>")

instance Pretty TermCond where
  ppr (LTerm ty) = text "terminator" <+> ppr ty
  ppr (LLen exp) = text "length"     <+> ppr exp


ppr_padsList itemTy sepTyOpt termTyOpt = brackets  (ppr itemTy <> ppr_sep sepTyOpt)   <> ppr_opt termTyOpt

ppr_opt opt = case opt of Nothing -> empty ; Just e -> (space <> ppr e)
ppr_optArgPat pat = case pat of Nothing -> empty; Just e -> (space <> pprHpat e)

ppr_optPred pred = case pred of Nothing -> empty; Just e -> (space <> text "where" <+> ppr e)

ppr_sep Nothing = empty
ppr_sep (Just sepTy) = text " |" <+> ppr sepTy




{- Utilities for generated pretty printer for PADS types -}
seplines :: Doc -> [Doc] -> Doc
seplines s = folddoc (\hd tl -> hd <> s </> tl)

whitesep = sep
field_ppr field_name ppr = text field_name   <+> equals <+> ppr
record_ppr str pprs  = namedty_ppr str (recordbody_ppr pprs)
recordbody_ppr docs = braces (align (seplines comma docs))

tuple_ppr ds = parens (align (commasep ds))

maybe_ppr d = case d of
  Nothing -> text "Nothing"
  Just a -> ppr a


namedty_ppr str ph = hang 2 (text str <+/> ph)

namedtuple_ppr :: String -> [Doc] -> Doc
namedtuple_ppr name pprls = group $ hang 2 (text name <+/> (tuple_ppr pprls))


list_ppr ds = (text "[---" <//>
                    align (seplines comma ds ) <//>
                text "]")

--instance (Pretty a, Pretty b)  => Pretty (M.Map a b) where
--  ppr = map_ppr
map_ppr d = list_ppr (map ppr (M.toList d))

string_ppr :: String -> Doc
string_ppr = ppr


namedlist_ppr :: String -> [Doc] -> Doc
namedlist_ppr name pprls = group $ hang 2 (text name <+/> (list_ppr pprls))
