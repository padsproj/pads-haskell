{-# LANGUAGE TupleSections, ViewPatterns, TemplateHaskell, NamedFieldPuns, ScopedTypeVariables,
             RecordWildCards, UndecidableInstances #-}
{-# OPTIONS_HADDOCK prune #-}
{-|
  Module      : Language.Pads.CodeGen
  Description : Template Haskell based code generator
  Copyright   : (c) 2011
                Kathleen Fisher <kathleen.fisher@gmail.com>
                John Launchbury <john.launchbury@gmail.com>
  License     : MIT
  Maintainer  : Karl Cronburg <karl@cs.tufts.edu>
  Stability   : experimental

  To the best of my knowledge, all functions defined herein are only ever run at
  compile time. These compile time functions are intended to be used in a
  quasiquoted context where the runtime system support modules have been properly
  imported. See "Examples.First" for the necessary imports.

  The crucial piece of the code generator is 'genParseTy', which translates Pads
  syntactic forms into Haskell code for parsing them.

-}
module Language.Pads.CodeGen where

import Language.Pads.Syntax as PS
import Language.Pads.MetaData
import Language.Pads.Generic
import Language.Pads.PadsParser
import Language.Pads.CoreBaseTypes
import Language.Pads.TH
import qualified Language.Pads.Errors as E
import qualified Language.Pads.Source as S
import Language.Pads.PadsPrinter
import Language.Pads.Generation

import Language.Haskell.TH
-- import Language.Haskell.TH.Syntax

import Data.Data
import Data.Char
import qualified Data.Map as M
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Control.Monad
import Language.Haskell.TH.Syntax --(lift, Name, NameFlavour(..), OccName)
import qualified System.Random.MWC as MWC
import qualified Debug.Trace as D

-- |
type BString = S.RawStream

-- | A function passed into the code generator which gets called on data type
-- declarations and returns a list of standalone-deriving declarations.
-- Presently this is unused by Pads.
type Derivation = Dec -> Q [Dec]

-- | Top level code gen function from Pads decls to Haskell decls
make_pads_declarations :: [PadsDecl] -> Q [Dec]
make_pads_declarations = make_pads_declarations' (const $ return [])

-- | Top level code gen function from Pads decls to Haskell expression with just
-- the PADS AST (no parser codegen)
make_pads_asts :: [PadsDecl] -> Q Exp
make_pads_asts = let
    mpa pd@(PadsDeclType n _ _ _)     = [| ($(litE $ stringL n), $(lift pd)) |]
    mpa pd@(PadsDeclData n _ _ _ _)   = [| ($(litE $ stringL n), $(lift pd)) |]
    mpa pd@(PadsDeclNew n _ _ _ _)    = [| ($(litE $ stringL n), $(lift pd)) |]
    mpa pd@(PadsDeclObtain n _ _ _ _) = [| ($(litE $ stringL n), $(lift pd)) |]
  in listE . (map mpa)

-- | Top level code gen function from Pads decls to Haskell decls with the
-- specified list of type classes for all of the generated Pads types to derive.
make_pads_declarations' :: Derivation -> [PadsDecl] -> Q [Dec]
make_pads_declarations' derivation ds = fmap concat (mapM (genPadsDecl derivation) ds)

-------------------------------------------------------------------------------
-- * Generating Declarations and Code from Individual Pads Declarations

genPadsDecl :: Derivation -> PadsDecl -> Q [Dec]
-- ^ Generate all the top level Haskell declarations associated with a single
-- Pads declaration.
genPadsDecl derivation pd@(PadsDeclType name args pat padsTy) = do
  let typeDecs = mkTyRepMDDecl name args padsTy
  parseM  <- genPadsParseM name args pat padsTy
  parseS  <- genPadsParseS name args pat
  printFL <- genPadsPrintFL name args pat padsTy
  genM    <- genPadsGenM name args pat padsTy
  serialize <- genPadsSerialize name args pat padsTy
  def <- genPadsDef name args pat padsTy
  let sigs = mkPadsSignature name args (fmap patType pat)
  ast <- astDecl name pd
  return $ typeDecs ++ parseM ++ parseS ++ printFL ++ genM ++ serialize ++ def ++ sigs

genPadsDecl derivation pd@(PadsDeclData name args pat padsData derives) = do
  dataDecs <- mkDataRepMDDecl derivation name args padsData derives
  parseM <- genPadsDataParseM name args pat padsData
  parseS <- genPadsParseS name args pat
  printFL <- genPadsDataPrintFL name args pat padsData
  genM <- genPadsDataGenM name args pat padsData
  serialize <- genPadsDataSerialize name args pat padsData
  def <- genPadsDataDef name args pat padsData
  let instances = mkPadsInstance name args (fmap patType pat)
  let sigs = mkPadsSignature name args (fmap patType pat)
  ast <- astDecl name pd
  return $ dataDecs ++ parseM ++ parseS ++ printFL ++ genM ++ serialize ++ def ++ instances ++ sigs

genPadsDecl derivation pd@(PadsDeclNew name args pat branch derives) = do
  dataDecs <- mkNewRepMDDecl derivation name args branch derives
  parseM <- genPadsNewParseM name args pat branch
  parseS <- genPadsParseS name args pat
  printFL <- genPadsNewPrintFL name args pat branch
  genM <- genPadsNewGenM name args pat branch
  serialize <- genPadsNewSerialize name args pat branch
  def <- genPadsNewDef name args pat branch
  let instances = mkPadsInstance name args (fmap patType pat)
  let sigs = mkPadsSignature name args (fmap patType pat)
  ast <- astDecl name pd
  return $ dataDecs ++ parseM ++ parseS ++ printFL ++ genM ++ serialize ++ def ++ instances ++ sigs

genPadsDecl derivation pd@(PadsDeclObtain name args padsTy exp genM) = do
  let mdDec = mkObtainMDDecl name args padsTy
  parseM  <- genPadsObtainParseM name args padsTy exp
  parseS  <- genPadsParseS name args Nothing
  printFL <- genPadsObtainPrintFL name args padsTy exp
  genM <- genPadsObtainGenM name args padsTy exp genM
  serialize <- genPadsObtainSerialize name args padsTy exp
  def <- genPadsObtainDef name args padsTy exp
  let sigs = mkPadsSignature name args Nothing
  ast <- astDecl name pd
  return $ mdDec ++ parseM ++ parseS ++ printFL ++ genM ++ serialize ++ def ++ sigs

-- | A Haskell declaration containing the literal Pads AST representation of a
-- Pads description (the syntax of Pads encoded as Haskell data constructors)
astDecl name pd = funD (mkName $ "ast_" ++ name) [clause [] (normalB $ lift pd) []]

-- | The Haskell 'Type' of a Haskell pattern 'Pat'.
patType :: Pat -> Type
patType p = case p of
  LitP lit -> case lit of
                CharL c   -> VarT ''Char
                StringL s -> VarT ''String
  TupP ps  -> mkTupleT (map patType ps)
  SigP p t -> t
  ParensP p -> patType p
  otherwise -> error $ show p

-------------------------------------------------------------------------------
-- * Generating Rep/MD Type Declarations

-- | Make the type declarations for the representation and the metadata of a
-- Pads-defined type, @'PadsTy'@.
mkTyRepMDDecl :: UString -> [UString] -> PadsTy -> [Dec]
mkTyRepMDDecl name args ty = [repType, mdType]
  where
  repType = TySynD (mkRepName name) tyArgs (mkRepTy ty)
  mdType  = TySynD (mkMDName name) tyArgsMD (mkMDTy False ty)
  tyArgs  = map (PlainTV . mkName) args
  tyArgsMD  = map (PlainTV . mkName . (++"_md")) args

-------------------------------------------------------------------------------
-- * Generating Rep/MD Data Declarations

-- | Make the data type declarations for the representation and the metadata of
-- a Pads-defined data type, @'PadsData'@.
mkDataRepMDDecl :: Derivation -> UString -> [LString] -> PadsData -> [QString] -> Q [Dec]
mkDataRepMDDecl derivation name args branches ds = do
  bs' <- mapM (return . mkMDUnion) bs
  imdDecl  <- dataD (cxt []) (mkIMDName name) tyArgsMD Nothing bs'  [derive []]
  bs'' <- mapM (return . mkRepUnion) bs
  --let ds' = map (conT . mkName . qName) ds
  dataDecl <- dataD (cxt []) (mkRepName name) tyArgs   Nothing bs'' [derive ds]
  derivesData <- derivation dataDecl
  derivesImd <- derivation imdDecl
  let mdName = mkMDName name
  --let bT = bangType (mkStrict NotStrict)
  --let mdDeclConstr = normalC mdName $ [bT $ return $ mkTupleT [ConT '' Base_md, imdApp]]
  --mdDecl <- newtypeD (cxt []) mdName tyArgsMD Nothing mdDeclConstr []
  let mdDecl   = TySynD   (mkMDName name)  tyArgsMD (mkTupleT [ConT ''Base_md, imdApp])
  return $ [dataDecl, mdDecl, imdDecl] ++ derivesData ++ derivesImd
  where
    tyArgs   = map (PlainTV . mkName) args
    tyArgsMD = map (PlainTV . mkName . (++"_md")) args
    imdApp   = foldl AppT (ConT (mkIMDName name)) (map (VarT . mkName . (++"_md")) args)
    bs       = case branches of
                 PUnion bnchs    -> bnchs
                 PSwitch exp pbs -> [b | (p,b) <- pbs]

-- | Convert a Pads strictness annotation into the appropriate Haskell
-- strictness annotation in the template haskell Q monad for splicing.
mkStrict :: PadsStrict -> Q Strict
mkStrict NotStrict  = bang noSourceUnpackedness noSourceStrictness  -- i.e. notStrict
mkStrict IsStrict   = bang noSourceUnpackedness sourceStrict        -- i.e. isStrict

-- | Make the Haskell data type *constructor* (@'normalC'@ and @'recC'@) for the
-- given fragment of a Pads type (@'BranchInfo'@).
mkRepUnion :: BranchInfo -> ConQ
mkRepUnion (BConstr c args expM) = normalC (mkConstrName c) reps
  where reps = [bangType (mkStrict strict) (return $ mkRepTy ty) | (strict,ty) <- args, hasRep ty]
mkRepUnion (BRecord c fields expM) = recC (mkConstrName c) lreps
  where lreps = [ varBangType
                    (mkName l)
                    (bangType (mkStrict strict)
                              (return $ mkRepTy ty))
                | (Just l,(strict,ty),_,_) <- fields, hasRep ty]

-- | Make the 'Con' metadata constructor definition for an individual branch of
-- a Pads type, which gets used to create the Haskell data type declaration for
-- the metadata of a Pads type.
mkMDUnion :: BranchInfo -> Q Con
mkMDUnion (BConstr c args expM) = normalC (mkConstrIMDName c) mds
  where
    mds = [bangType (mkStrict NotStrict) (return $ mkMDTy False ty) | (_,ty) <- args] --MD , hasRep ty]
mkMDUnion (BRecord c fields expM) = do
  { let lmds = [ do { fn <- genLabMDName "m" lM
                    ; varBangType fn (bangType (mkStrict NotStrict) (return $ mkMDTy False ty))
                    }
               | (lM,(_,ty),_,_) <- fields
               ]
  ; recC (mkConstrIMDName c) lmds
  }
--MD    lmds <- return [(mkFieldMDName l,NotStrict,mkMDTy ty) | (Just l,(_,ty),_) <- fields, hasRep ty]

-- | Make the type context of a data declaration, consisting of the typeclasses
-- instanced by Pads data types.
--derive :: [QString] -> CxtQ
derive :: [QString] -> DerivClauseQ
derive ds = derivClause Nothing $ map (conT . mkName . qName) ds
  ++ [conT $ mkName d | d<-["Show","Eq","Typeable","Data","Ord"], not (d `elem` map last ds)]

-------------------------------------------------------------------------------
-- * Generating Rep/MD Newtype Declarations

-- | Construct the newtype Haskell data declaration from a Pads type defined
-- using the "newtype" keyword.
mkNewRepMDDecl :: Derivation -> UString -> [LString] -> BranchInfo -> [QString] -> Q [Dec]
mkNewRepMDDecl derivation name args branch ds = do
  imdDecl  <- newtypeD (cxt []) (mkIMDName name) tyArgsMD Nothing (mkMDUnion  branch) [derive []]
  let ds' = map (conT . mkName . qName) ds
  dataDecl <- newtypeD (cxt []) (mkRepName name) tyArgs   Nothing (mkRepUnion branch) [derive ds]
  --[derivClause Nothing ds']
  derivesData <- derivation dataDecl
  derivesImd <- derivation imdDecl
  return $ [dataDecl, mdDecl, imdDecl] ++ derivesData ++ derivesImd
  where
    mdDecl   = TySynD   (mkMDName name)  tyArgsMD (mkTupleT [ConT ''Base_md, imdApp])
    tyArgs   = map (PlainTV . mkName) args
    tyArgsMD   = map (PlainTV . mkName . (++"_md")) args
    imdApp   = foldl AppT (ConT (mkIMDName name)) (map (VarT . mkName . (++"_md")) args)

-------------------------------------------------------------------------------
-- * Generating MD Type from Obtain Declarations
-- Design decision not to do this.

-- | Construct the Haskell type synonym declaration for a Pads type declared
-- using the "obtain" keyword.
mkObtainMDDecl :: UString -> [UString] -> PadsTy -> [Dec]
mkObtainMDDecl name args ty
  = [mdType]
  where
    mdType  = TySynD (mkMDName name) tyArgsMD (mkMDTy False ty)
    tyArgsMD  = map (PlainTV . mkName . (++"_md")) args

-------------------------------------------------------------------------------
-- * Generating Representation Type of a Type Expression

-- | Make the template haskell 'Type' for the given 'PadsTy' pads type, to be
-- used anywhere in generated Haskell code where the representation type is
-- expected.
mkRepTy ::  PadsTy -> Type
mkRepTy ty = case ty of
  PPartition pty exp          -> mkRepTy pty
  PConstrain pat pty exp      -> mkRepTy pty
  PTransform tySrc tyDest exp _ -> mkRepTy tyDest
  PList ty sep term           -> ListT `AppT` mkRepTy ty
  PValue exp pty              -> mkRepTy pty
  PApp tys expM               -> foldl1 AppT [mkRepTy ty | ty <- tys, hasRep ty]
  PTuple tys                  -> mkRepTuple tys
  PExpression _               -> ConT ''()
  PTycon c                    -> ConT (mkRepQName c)
  PTyvar v                    -> VarT (mkName v)

-- | Make the template haskell 'Type' corresponding to a tuple consisting of the
-- given pads types given in list form at compile time '[PadsTy]'.
mkRepTuple :: [PadsTy] -> Type
mkRepTuple tys = case reps of
    []     -> ConT ''()
    [ty]   -> ty
    (t:ts) -> mkTupleT reps
  where
    reps = [mkRepTy ty | ty <- tys, hasRep ty]

-------------------------------------------------------------------------------
-- * Generating Meta-Data Representation of Type Expression

-- | Make the template haskell 'Type' corresponding to the externally visible
-- metadata of a given 'PadsTy'. The boolean indicates whether or not Pads type
-- variables 'PTyvar's should be put in a 'Meta' constructor or merely stuffed
-- into a 'VarT' and appended with "_md" postfix. Currently we always do the
-- latter (all calls to 'mkMDTy' give False as the boolean).
mkMDTy :: Bool -> PadsTy -> Type
mkMDTy isMeta ty = case ty of
  PPartition pty exp      -> mkMDTy isMeta pty
  PConstrain pat pty exp  -> mkMDTy isMeta pty
  PTransform src dest exp _ -> mkMDTy isMeta dest
  PList ty sep term       -> mkTupleT [ConT ''Base_md, ListT `AppT` mkMDTy isMeta ty]
  PValue exp pty          -> mkMDTy isMeta pty
  PApp tys expM           -> foldl1 AppT [mkMDTy isMeta ty | ty <- tys] --MD , hasRep ty]
  PTuple tys              -> mkMDTuple isMeta tys
  PExpression _           -> ConT ''Base_md
  PTycon c                -> ConT (mkMDQName c)
  PTyvar v                -> if isMeta
    then AppT (ConT ''Meta) (VarT $ mkName v)
    else VarT (mkName $ v ++ "_md")

-- | Make the template haskell 'Type' corresponding to a Haskell tuple type
-- consisting of the metadata types for the given Pads types '[PadsTy]'.
mkMDTuple :: Bool -> [PadsTy] -> Type
mkMDTuple isMeta tys = case mds of
    []     -> ConT ''Base_md
    [m]    -> mkTupleT [ConT ''Base_md, m]
    (m:ms) -> mkTupleT [ConT ''Base_md, mkTupleT mds]
  where
    mds = [mkMDTy isMeta ty | ty <- tys] --MD , hasRep ty]


-------------------------------------------------------------------------------
-- * Generating Instance Declarations from Data / New Declarations

-- | Make the following instance and type instance declarations for a Pads data
-- type and new type declaration:
--
-- > [pads| data Foo (Bar1, Bar2, Bar3) = Foo
-- >    { i1 :: Bar1
-- >    , i2 :: Bar2 i1
-- >    , i3 :: Bar3 i2
-- >    } |]
--
-- > instance Pads1 (Bar1, Bar2, Bar3) Foo Foo_md where
-- >   parsePP1 = foo_parseM
-- >   printFL1 = foo_printFL
-- >   def1     = foo_def
-- > type instance Meta Foo = Foo_md
-- > type instance PadsArg Foo = (Bar1, Bar2, Bar3)
mkPadsInstance :: UString -> [LString] -> Maybe Type -> [Dec]
mkPadsInstance str args mb@(Nothing)
  = buildInst mb str args (ConT ''Pads1 `AppT` TupleT 0)
mkPadsInstance str args mb@(Just ety)
  = buildInst mb str args (ConT ''Pads1 `AppT` ety)

-- | See 'mkPadsInstance' above.
buildInst mb str args pads =
    [ InstanceD Nothing ctx inst [parsePP_method, printFL_method,def_method]
    , TySynInstD ''Meta $ TySynEqn [ty_name] meta_ty
    , TySynInstD ''PadsArg $ TySynEqn [ty_name] arg_ty
    ]
  where
  arg_ty = case mb of
    Nothing -> TupleT 0
    Just ety -> ety
  mbarg = case mb of
    Nothing -> [TupP []]
    Just _ -> []
  inst    = applyT [pads, ty_name, md_ty]
  ty_name = applyT (ConT (mkName str) : map fst argpairs)
  md_ty   = applyT (ConT (mkMDName str) : map snd argpairs)
  meta_ty   = applyT (ConT (mkMDName str) : metas)
  parsePP_method = FunD 'parsePP1 [Clause mbarg (NormalB (applyE (VarE (mkTyParserName str) : [VarE 'parsePP | a <- args]))) []]
  printFL_method =
    if str == "Entry"
      then FunD 'printFL1 [Clause mbarg (NormalB $ VarE $ mkName "undefined") []]
      else FunD 'printFL1 [Clause mbarg (NormalB (applyE (VarE (mkTyPrinterName str) : [VarE 'printFL | a <- args]))) []]
  def_method = FunD 'def1 [Clause mbarg (NormalB (applyE (VarE (mkTyDefName str) : [VarE 'def | a <- args]))) []]
  argpair n = (VarT (mkName n),VarT (mkName $ n++"_md"))
  meta n = AppT (ConT ''Meta) (VarT $ mkName n)
  argpairs = [argpair a | a <- args]
  metas = map meta args
  argtyvars = concat [[PlainTV (mkName a), PlainTV (mkName (a++"_md"))] | a <- args]

  ctx = [AppT (AppT (ConT ''Pads) r) m | (r,m) <- argpairs]

  padsprinter t t_md = AppT (ConT ''PadsPrinter) $ appT2 (TupleT 2) t t_md

  printer = case mb of
    Nothing -> padsprinter ty_name md_ty
    Just ety -> appT2 ArrowT ety (padsprinter ty_name md_ty)


-- | Make the following type signatures, applicable for all the forms of a Pads
-- declaration:
--
-- > foo_printFL :: (Bar1, Bar2, Bar3) -> PadsPrinter (Foo, Foo_md)
-- > foo_def     :: (Bar1, Bar2, Bar3) -> Foo
--
-- See 'mkPadsInstance' above for the definition of the Pads type "Foo".
mkPadsSignature :: UString -> [LString] -> Maybe Type -> [Dec]
mkPadsSignature str args mb@(Nothing)
  = buildSignature mb str args (ConT ''Pads)
mkPadsSignature str args mb@(Just ety)
  = buildSignature mb str args (ConT ''Pads1 `AppT` ety)

-- | See 'mkPadsSignature' above.
buildSignature mb str args pads =
  if str == "Entry"
    then [def_signature]
    else [printFL_signature,def_signature]
  where
  mbarg = case mb of
    Nothing -> [TupP []]
    Just _ -> []
  inst    = applyT [pads, ty_name, md_ty]
  ty_name = applyT (ConT (mkName str) : map (\(x,y,z) -> y) argpairs)
  md_ty   = applyT (ConT (mkMDName str) : map (\(x,y,z) -> z) argpairs)
  meta_ty   = applyT (ConT (mkMDName str) : metas)
  argpair n = (VarT (mkName $ n++"_arg"),VarT (mkName n),VarT (mkName $ n++"_md"))
  meta n = AppT (ConT ''Meta) (VarT $ mkName n)
  argpairs = [argpair a | a <- args]
  metas = map meta args
  argtyvars = concat [[PlainTV (mkName (a++"_arg")),PlainTV (mkName a), PlainTV (mkName (a++"_md"))] | a <- args]

  printerctx = concat $ [[AppT (ConT ''Data) r, AppT (ConT ''Data) m] | (arg,r,m) <- argpairs]
  defctx = concat $ [[AppT (ConT ''Data) r] | (arg,r,m) <- argpairs]

  padsprinter t t_md = AppT (ConT ''PadsPrinter) $ appT2 (TupleT 2) t t_md
  padsdef t t_md = t

  printer = case mb of
    Nothing -> padsprinter ty_name md_ty
    Just ety -> appT2 ArrowT ety (padsprinter ty_name md_ty)
  def = case mb of
    Nothing -> padsdef ty_name md_ty
    Just ety -> appT2 ArrowT ety (padsdef ty_name md_ty)

  printFL_signature = SigD (mkTyPrinterName str) $ ForallT argtyvars printerctx $ foldr (\a t -> let (a_arg,a_rep,a_md) = argpair a in appT2 ArrowT (padsprinter a_rep a_md) t) printer args
  def_signature = SigD (mkTyDefName str) $ ForallT argtyvars defctx $ foldr (\a t -> let (a_arg,a_rep,a_md) = argpair a in appT2 ArrowT (padsdef a_rep a_md) t) def args

-------------------------------------------------------------------------------
-- * Generating Parser Declaration from Type / Data / New Declarations

-- | Construct the function body and resulting declaration of the "_parseM"
-- function for a given 'PadsTy' type declaration.
genPadsParseM :: UString -> [LString] -> Maybe Pat -> PadsTy -> Q [Dec]
genPadsParseM name args patM padsTy = do
  let body = genParseTy padsTy
  mkParserFunction name args patM body

-- | 'PadsData' data declaration flavour of the "_parseM" function.
genPadsDataParseM :: UString -> [LString] -> (Maybe Pat) -> PadsData -> Q [Dec]
genPadsDataParseM name args patM padsData = do
  let body = genParseData padsData
  mkParserFunction name args patM body

-- | 'BranchInfo' new type declaration flavour of the "_parseM" function.
genPadsNewParseM :: UString -> [LString] -> (Maybe Pat) -> BranchInfo -> Q [Dec]
genPadsNewParseM name args patM branch = do
  (dec,exp) <- genParseBranchInfo branch
  let body = letE [return dec] (return exp)
  mkParserFunction name args patM body

-- | Pads Obtain declaration flavour of the "_parseM" function.
genPadsObtainParseM :: UString -> [LString] -> PadsTy -> Exp -> Q [Dec]
genPadsObtainParseM name args padsTy exp = do
  let body = genParseTy (PTransform padsTy (PTycon [name]) exp Nothing)
  mkParserFunction name args Nothing body

-- | Construct the declaration for a function which monadically parses a Pads
-- type given the body of the function as input.
mkParserFunction :: UString -> [LString] -> Maybe Pat -> Q Exp -> Q [Dec]
mkParserFunction name args patM body
  = sequence $ if name == "Entry" then [sig,fun] else [fun]
  where
    fun        = funD parserName [clause parserArgs (normalB body) []]
    sig        = sigD parserName [t| PadsParser ($(conT $ mkConstrName name), (Base_md, $(conT $ mkConstrIMDName name))) |]
    parserName = mkTyParserName name
    parserArgs = map (varP . mkVarParserName) args ++ Maybe.maybeToList (return <$> patM)

-------------------------------------------------------------------------------
-- * Generating String-Parser Declaration

-- | Construct the "_parseS" function at compile time such that it makes a call
-- to 'parseStringInput' at runtime.
genPadsParseS :: UString -> [LString] -> Maybe Pat -> Q [Dec]
genPadsParseS name args patM = do
  { body <- [| parseStringInput $(return parserWithArgs) |]
  ; return [ FunD (mkTyParserSName name) [Clause parserArgs (NormalB body) []] ]
  }
  where
    parserWithArgs = foldl1 AppE (VarE parserName : map patToExp parserArgs)
    parserName     = mkTyParserName name
    parserArgs     = map (VarP . mkVarParserName) args ++ Maybe.maybeToList patM

-------------------------------------------------------------------------------
-- * Generating Parser from Type Expression

-- | This function only ever gets called at compile time in order to construct a
-- template haskell expression to be used somewhere in the body of a "_parseM"
-- function. This expression is the meat of the pads-haskell parsing algorithm
-- and semantics - we use metaprogramming to map the Pads syntax onto
-- expressions which return a tuple consisting of the parsed representation
-- followed by the metadata (with parse errors).
genParseTy :: PadsTy -> Q Exp
genParseTy pty = case pty of
    PConstrain pat ty exp   -> genParseConstrain (return pat) ty (return exp)
    PTransform src dest exp _ -> genParseTyTrans src dest (return exp)
    PList ty sep term       -> genParseList ty sep term
    PPartition ty exp       -> genParsePartition ty exp
    PValue exp ty           -> genParseValue exp
    PApp tys argE           -> genParseTyApp tys argE
    PTuple tys              -> genParseTuple tys
    PExpression exp         -> genParseExp exp
    PTycon c                -> return $ mkParseTycon c
    PTyvar v                -> return $ mkParseTyvar v


-- | Simply generate a call to the runtime system function 'parseConstraint'
-- where the first argument is a Haskell expression spliced directly into the
-- call to 'parseConstraint' which parses the thing being constrained and the
-- second argument is the (Haskell) predicate function used to constrain the
-- Pads type.
genParseConstrain :: Q Pat -> PadsTy -> Q Exp -> Q Exp
genParseConstrain patQ ty expQ = [| parseConstraint $(genParseTy ty) $pred |]
  where
    pred = lamE [patQ, varP (mkName "md")] expQ


-- | Simply generate a call to the runtime system function 'parseTransform'
-- where the first argument is the spliced-in-place parser for the "source" Pads
-- type being transformed and the second argument is the (Haskell)
-- transformation function for producing something of the desired destination
-- type. Note that we can ignore the destination 'PadsTy' at compile time in
-- *this* function because the Haskell type checker will type check the result
-- of 'parseTransform' for us.
genParseTyTrans :: PadsTy -> PadsTy -> Q Exp -> Q Exp
genParseTyTrans src dest expQ
  = [| parseTransform $(genParseTy src) (fst $expQ) |]

-- | This compile time function figures out which runtime system support
-- function to generate a call to for parsing a Pads list type based on the
-- given separator Pads type and the desired termination condition 'TermCond'.
genParseList :: PadsTy -> (Maybe PadsTy) -> (Maybe TermCond) -> Q Exp
genParseList ty sep term =
  case (sep,term) of
    (Nothing,  Nothing)          -> [| parseListNoSepNoTerm $(genParseTy ty) |]
    (Just sep, Nothing)          -> [| parseListSepNoTerm $(genParseTy sep) $(genParseTy ty) |]
    (Nothing,  Just (LLen lenE)) -> [| parseListNoSepLength $(return lenE) $(genParseTy ty) |]
    (Just sep, Just (LLen lenE)) -> [| parseListSepLength $(genParseTy sep) $(return lenE) $(genParseTy ty) |]
    (Nothing,  Just (LTerm term))-> [| parseListNoSepTerm $(genParseTy term) $(genParseTy ty) |]
    (Just sep, Just (LTerm term))-> [| parseListSepTerm $(genParseTy sep) $(genParseTy term) $(genParseTy ty) |]


-- | Simply generate a call to the runtime system function 'parsePartition'
-- where the first argument is an expression for parsing the 'PadsTy' pads type
-- we're partitioning on and the second argument is the Haskell expression given
-- in the Pads syntactic form specifying the record discipline with which to
-- partition things. For example the following code:
--
-- > type Foo = (partition [Bar] using none)
--
-- declares a type Foo which is a list of Bars where Bars are separated by
-- nothing.
genParsePartition :: PadsTy -> Exp -> Q Exp
genParsePartition ty disc = [| parsePartition $(genParseTy ty) $(return disc) |]

-- | This compile time function generates code which wraps a Pads Value type's
-- Haskell expression in the appropriate type to be returned for use in the pads
-- parsing monad, namely of type 'PadsParser (rep, md)' where rep and md are the
-- representation and metadata type variables.
genParseValue :: Exp -> Q Exp
genParseValue exp = [| return ($(return exp), cleanBasePD) |]
--genParseValue exp = return $ AppE (VarE 'return) (TupE [exp,VarE 'cleanBasePD])

-- | Construct the sequentially-defined parser for a Pads tuple type.
genParseTuple :: [PadsTy] -> Q Exp
genParseTuple []  = [| return ((), cleanBasePD) |]
genParseTuple tys = do
  f_rep_name <- newName "f_rep"
  f_md_name  <- newName "f_md"
  let f_rep     = buildF_rep      f_rep_name vars_frep
      f_rep_sig = buildF_rep_sig  f_rep_name sigs_frep
      f_md      = buildF_md       f_md_name  vars_fmd
  --f_md_sig <- buildF_md_sig       f_md_name  tys
  body  <- foldl parseNext [| return ($(dyn "f_rep"),$(dyn "f_md")) |] tys
  return (LetE [f_rep_sig,f_rep {-,f_md_sig-},f_md] body)
  where
    vars_frep = [v | (v,t) <- zip vars_fmd tys, hasRep t]
    sigs_frep = [t | t <- tys, hasRep t]
    vars_fmd  = [ mkName ("x"++show n) | n <- [1 .. length tys]]

-- | Glom the generated parser for the given 'PadsTy' onto the given parser
-- using the '=@=' and '=@' runtime system operators.
parseNext :: Q Exp -> PadsTy -> Q Exp
parseNext prog t
  | hasRep t  = [| $prog =@= $(genParseTy t) |]
  | otherwise = [| $prog =@  $(genParseTy t) |]

-- | Construct the "f_rep" let-bound function inside of a Pads tuple type for
-- uncurrying the result of parsing the tuple sequentially at runtime. The
-- "f_rep" function generated by *this* function gets passed into the '=@=' and
-- '=@' runtime system operators which call f_rep on the result of parsing each
-- of the members of the tuple.
buildF_rep :: Name -> [Name] -> Dec
buildF_rep name vars_frep
  = FunD name [Clause
         (map VarP vars_frep) (NormalB (TupE (map VarE vars_frep))) [] ]

isVarT (VarT _) = True
isVarT _        = False

findPTyVars :: [PadsTy] -> [Name]
findPTyVars ptys = let

    varTs' :: PadsTy -> [String]
    varTs' (PTyvar s) = [s]
    varTs' t          = varTs t

    varTs (PConstrain _ t _) = varTs' t
    varTs (PTransform t1 t2 _ _) = varTs' t1 ++ varTs' t2
    varTs (PList t1 (Just t2) _) = varTs' t1 ++ varTs' t2
    varTs (PList t1 Nothing _) = varTs' t1
    varTs (PPartition t _) = varTs' t
    varTs (PValue _ t) = varTs' t
    varTs (PApp ts _) = concatMap varTs ts
    varTs _ = []

  in List.nub $ concatMap (map mkName . varTs) ptys

buildF_md_sig :: Name -> [PadsTy] -> Q Dec
buildF_md_sig name ptys = do
  let tys   = map (mkMDTy False) ptys
      mdRet = foldl AppT (TupleT $ length tys) tys
  retTy <- [t| (Base_md, $(return mdRet)) |]
  let sigTy = foldr1 (appT2 ArrowT) (tys ++ [retTy])
      ptyVarNames = findPTyVars ptys --filter isVarT tys
      varTTys = map VarT ptyVarNames
      varTNames = map PlainTV ptyVarNames
      sigT' = ForallT varTNames (map (AppT (ConT ''PadsMD)) varTTys
                              ++ map (AppT (ConT ''Data))   varTTys) sigTy
  do D.traceM $ "buildF_md_sig] " ++ show name ++ " \n " ++ show varTNames ++ " \n "
        ++ show tys ++ " \n " ++ show ptys
     return (case tys of
            []     -> SigD name $ TupleT 0
            (t:[]) -> SigD name $ appT2 ArrowT t t
            _      -> SigD name $ sigT')

-- | Same as 'buildF_rep' above but for the metadata instead of the parse
-- representation. In this case we need to pull off just the 'Base_md' from the
-- metadata resulting from whatever the parser returned to us for each of the
-- tuple results using the 'get_md_header' type class function provided by the
-- runtime system.
buildF_md :: Name -> [Name] -> Dec
buildF_md f_md_name vars_fmd
  = FunD f_md_name [Clause (map VarP vars_fmd) (NormalB body) []]
  where
    mdHeaders = [ VarE 'get_md_header `AppE` VarE xi | xi <- vars_fmd ]
    body = TupE [mkMergeBaseMDs mdHeaders, TupE (map VarE vars_fmd)]

buildF_rep_sig :: Name -> [PadsTy] -> Dec
buildF_rep_sig name ptys = let
    tys   = map mkRepTy ptys
    retTy = foldl  AppT (TupleT $ length tys) tys
    sigTy = foldr1 (appT2 ArrowT) (tys ++ [retTy])
  in (case tys of
        []     -> SigD name $ TupleT 0
        (t:[]) -> SigD name $ appT2 ArrowT t t
        _      -> SigD name $ sigTy)

-- | Generate a call to 'mergeBaseMDs'
mkMergeBaseMDs :: [Exp] -> Exp
mkMergeBaseMDs [e] = e
mkMergeBaseMDs es  = VarE 'mergeBaseMDs `AppE` ListE es

-- | Construct a call to the 'litParse' runtime system type class function so
-- that we can parse a literal (Haskell) expression. The type of the expression
-- provided as a Haskell expression must be Literally Parseable ('LitParse' type
-- class), otherwise the code generated by *this* compile time function produces
-- a type error.
genParseExp :: Exp -> Q Exp
genParseExp exp                = [| litParse $(return exp) |]

-- | Generate the parser for a Pads type application.
genParseTyApp :: [PadsTy] -> Maybe Exp -> Q Exp
genParseTyApp tys expM = do
  fs <- mapM genParseTy tys
  return (foldl1 AppE (fs ++ Maybe.maybeToList expM))

-- | Make the parser for a Pads type constructor - just return it as a Haskell
-- variable expression.
mkParseTycon :: QString -> Exp
mkParseTycon ["EOF"] = VarE 'eof_parseM
mkParseTycon ["EOR"] = VarE 'eor_parseM
mkParseTycon c       = VarE (mkTyParserQName c)

-- | Make the parser for a Pads type variable - just return it as a Haskell
-- variable expression.
mkParseTyvar :: String -> Exp
mkParseTyvar v = VarE (mkVarParserName v) -- should gensym these, but probably ok


-- * Generating Parsers from Union/Switch Expressions

-- | A data declaration in pads is either a union or a switch expression -
-- generate the template haskell for parsing them.
genParseData :: PadsData -> Q Exp
genParseData (PUnion bs)       = genParseUnion bs
genParseData (PSwitch exp pbs) = genParseSwitch exp pbs

-- | Generate the template haskell for parsing a Pads union expression. Namely
-- generate the metadata constructors for each of the branches of the union and
-- stuff them into let-bound functions so that nested parsers have them in
-- scope. Then generate a call to the runtime system function 'choiceP' for
-- choosing among the different parsers.
genParseUnion :: [BranchInfo] -> Q Exp
genParseUnion bs = do
  { (decs,bodies) <- fmap unzip $ mapM genParseBranchInfo bs
  ; let body = case bodies of
                 [b] -> b
                 bs  -> (VarE 'choiceP) `AppE` (ListE bs)
  ; return (LetE decs body)
  }

-- | Generate the template haskell case expression from a Pads switch type. This
-- is almost entirely just matching the syntax of a Pads case onto the syntax of
-- a Haskell case expression. Semantically the case just figures out which
-- parser needs to be run by pattern matching on something already parsed from
-- the input.
genParseSwitch :: Exp -> [(Pat,BranchInfo)] -> Q Exp
genParseSwitch exp pbs = do
  let (ps,bs) = unzip pbs
  (decs,bodies) <- fmap unzip $ mapM genParseBranchInfo bs
  let body = CaseE exp [Match p (NormalB b) [] | (p,b) <- zip ps bodies]
  return (LetE decs body)

-- | Generate the parser for an individual branch of a Pads new type, Pads
-- union, or Pads switch.
genParseBranchInfo :: BranchInfo -> Q (Dec,Exp)
genParseBranchInfo (BRecord c fields pred) = genParseRecord c fields pred
genParseBranchInfo (BConstr c args pred) = do
  { body <- foldl parseNext [| return ($(conE (mkConstrName c)),$(varE (mkfnMDName c))) |] tys
  ; return (con_md, body)
  }
  where
    tys  = [ty | (strict,ty) <- args]
    con_md = buildConstr_md (mkfnMDName c) (ConE (mkConstrIMDName c)) tys

-- | Build the constructor function for tupling together the metadata results of
-- parsing a bunch of Pads types.
buildConstr_md :: Name -> Exp -> [PadsTy] -> Dec
buildConstr_md fnMD conMD tys
  = FunD fnMD [Clause (map VarP vars_fmd) (NormalB body) []]
  where
    vars_fmd   = [ mkName ("x"++show n) | n <- [1 .. length tys]]
    mdHeaders  = [ VarE 'get_md_header `AppE` VarE xi | xi <- vars_fmd ]
    body       = TupE [mkMergeBaseMDs mdHeaders, applyE (conMD : map VarE vars_conmd)]
    vars_conmd = vars_fmd --MD [v | (v,t) <- zip vars_fmd tys, hasRep t]

-------------------------------------------------------------------------------
-- * Generating Parsers from Record Expressions

-- | Generate the template haskell code for parsing a Pads record.
genParseRecord :: UString -> [FieldInfo] -> (Maybe Exp) -> Q (Dec,Exp)
genParseRecord c fields pred = do
  c_md <- newName (strToLower c)
  let con_md = buildConstr_md c_md (ConE (mkConstrIMDName c))
                     [ty | (_,(_,ty),_,_) <- fields]
  labMDs  <- sequence [genLabMDName "x" l | (l,(_,_),_,_) <- fields]
  let fnMDLabs  = applyE $ map VarE (c_md : labMDs)
  doStmts <- sequence $ [genParseField f xn | (f,xn) <- zip fields labMDs]
  let labs = [mkName lab | (Just lab,(_,ty),_,_) <- fields, hasRep ty]
  let conLabs = applyE (ConE (mkConstrName c) : map VarE labs)
  returnStmt <- [| return ($(return conLabs),$(return fnMDLabs)) |]
  return (con_md, DoE (concat doStmts ++ [NoBindS returnStmt]))

-- | Generate the name (label?) for the metadata of a field in a record.
genLabMDName :: String -> Maybe String -> Q Name
genLabMDName s (Just lab) = return (mkFieldMDName lab)
genLabMDName s Nothing    = liftM mangleName (newName s)

-- | Generate the parser for a field of a Pads record.
genParseField :: FieldInfo -> Name -> Q [Stmt]
genParseField (labM, (strict, ty), expM,_) xn = do
  let parseTy = (case expM of
                    Nothing  -> genParseTy ty
                    Just exp -> genParseRecConstrain labP (varP xn) ty (return exp))
  sequence $
    [ bindS (tupP [labP, varP xn]) parseTy
    ]
  where
    labP = case labM of
              Just lab -> varP (mkName lab)
              Nothing  -> wildP

-- | Generate the parser for a constrained field on a record.
genParseRecConstrain :: Q Pat -> Q Pat -> PadsTy -> Q Exp -> Q Exp
genParseRecConstrain labP xnP ty exp = [| parseConstraint $(genParseTy ty) $pred |]
  where
    pred = lamE [labP, xnP] exp


-------------------------------------------------------------------------------
-- * Generating generation functions

-- * Generating Generator Declaration from Type / Data / New declarations

-- These functions largely mirror the structure of the above "ParseM"
-- functions, differing in the sort of function they output but sharing in
-- common how they construct said function.

-- | PadsDeclType generator declaration
genPadsGenM :: UString -> [LString] -> Maybe Pat -> PadsTy -> Q [Dec]
genPadsGenM name args patM padsTy = do
  let body = genGenTy padsTy
  mkGeneratorFunction name args patM body

-- | PadsDeclData generator declaration
genPadsDataGenM :: UString -> [LString] -> (Maybe Pat) -> PadsData -> Q [Dec]
genPadsDataGenM name args patM padsData = do
  let body = genGenData padsData
  mkGeneratorFunction name args patM body

-- | PadsDeclNew generator declaration
genPadsNewGenM :: UString -> [LString] -> (Maybe Pat) -> BranchInfo -> Q [Dec]
genPadsNewGenM name args patM branch = do
  exp <- genGenBranchInfo branch
  mkGeneratorFunction name args patM (return exp)

-- | PadsDeclObtain generator declaration - if the user provided a generator,
-- it will be included. If not, the type will lack a generator. If the user
-- includes a function conforming to the naming convention of a generator, i.e.
-- "name_genM" for a PadsTy called "Name," it is redundant (and in fact
-- erroneous) to include "generator name_genM," as this will result in an
-- attempted redefinition of name_genM as itself.
genPadsObtainGenM :: UString -> [LString] -> PadsTy -> Exp -> Maybe Exp -> Q [Dec]
genPadsObtainGenM name _ _ _ (Just gen) = mkGeneratorFunction name [] Nothing (return gen)
genPadsObtainGenM _    _ _ _ Nothing    = return []

-- | Create the actual generator function declaration for any PadsDecl flavor
mkGeneratorFunction :: UString -> [LString] -> Maybe Pat -> Q Exp -> Q [Dec]
mkGeneratorFunction name args patM body
  = sequence [fun]
  where
    fun = funD generatorName [clause generatorArgs (normalB body) []]
    generatorName = mkTyGeneratorName name
    generatorArgs = map (varP . mkVarGeneratorName) args ++ Maybe.maybeToList (return <$> patM)


-- * Generating Generators from Union/Switch Expressions

-- | Generate the generators for Pads data declarations.
genGenData :: PadsData -> Q Exp
genGenData (PUnion bs)       = genGenUnion bs
genGenData (PSwitch exp pbs) = do
  let matches = [match (return p) (normalB $ genGenBranchInfo b) [] | (p,b) <- pbs]
  caseE (return exp) matches

-- | Creates a runtime function which picks at random from  the generators for
-- each branch of the union, all of which are created here.
genGenUnion :: [BranchInfo] -> Q Exp
genGenUnion bs =
  case bs of
    [b] -> genGenBranchInfo b
    bs  -> do
      let bs' = map genGenBranchInfo bs
      index <- newName "index"
      dos <- newName "dos"
      bindList <- letS [valD (varP dos) (normalB (listE bs')) []]
      bindIndex <- bindS (varP index) [| randNumBound (length $(varE dos) - 1) |]
      indexList <- noBindS [| $(varE dos) !! $(varE index) |]
      return $ DoE [bindList,bindIndex,indexList]

-- | Dispatch to genGenRecord or genGenConstr
genGenBranchInfo :: BranchInfo -> Q Exp
genGenBranchInfo (BRecord c fields pred) = genGenRecord c fields pred
genGenBranchInfo (BConstr c args   pred) = genGenConstr c args   pred

-- | Generate the template Haskell code for generating a Pads record.
genGenRecord :: UString -> [FieldInfo] -> (Maybe Exp) -> Q Exp
genGenRecord c fields pred = do
  doStmts <- sequence $ map genGenField fields
  let labels = map mkName $ Maybe.catMaybes $ [label | (label,(_,ty),_,_) <- fields, hasRep ty]
  let conLabs = applyE (ConE (mkConstrName c) : map VarE labels)
  returnStmt <- [| (return :: a -> PadsGen a) ($(return conLabs)) |]
  return $ DoE (concat doStmts ++ [NoBindS returnStmt])

-- | Generate the generator for a field of a Pads record; each one becomes a
-- binding statement in a haskell do-expression.
genGenField :: FieldInfo -> Q [Stmt]
genGenField (labM, (strict, ty), expM, genM) = do
  let labP  = case labM of Nothing  -> wildP
                           Just lab -> varP $ mkName lab
  let genTy = case expM of Nothing  -> case genM of Just gen -> return gen; _ -> genGenTy ty
                           Just exp -> [| error "genGenField: parameterization via expression unsupported" |]
  sequence [bindS labP genTy]

-- | Generate the generator for a PADS data constructor (BConstr format of
-- BranchInfo).
genGenConstr :: String -> [ConstrArg] -> Maybe Exp -> Q Exp
genGenConstr c args pred = do
  let tys  = [ty | (_,ty) <- args]
  let tys' = map genGenTy (filter hasRep tys)
  names <- sequence [newName "x" | ty <- tys']
  binds <- sequence [bindS (varP n) ty | (n,ty) <- zip names tys']
  let constructor = (conE . mkName) c
  let toreturn = foldl1 appE (constructor : (map varE names))
  ret <- noBindS [| (return :: a -> PadsGen a) $toreturn |]
  return $ DoE (binds ++ [ret])

-- * Generating Generator from Type Expression

-- | Driver function for creating generators. Provided a PadsTy, it will return
-- a generator customized to work with that particular type.
genGenTy :: PadsTy -> Q Exp
genGenTy pty = case pty of
  PConstrain pat ty exp        -> genGenConstrain pat ty exp
  PTransform src dest exp genM -> genGenTransform src dest exp genM
  PList ty sep term            -> genGenList ty sep term
  PPartition ty exp            -> genGenTy ty
  PValue exp ty                -> genGenValue exp
  PApp tys argE                -> genGenTyApp tys argE
  PTuple tys                   -> genGenTuple tys
  PExpression exp              -> [| return $(return exp) |]
  PTycon c                     -> mkGenTycon c
  PTyvar v                     -> mkGenTyvar v

-- | Generate code that uses the runtime function 'untilM' to generate random
-- examples of data until one satisfies the constraint. If a predicate
-- requires that the variable in question be exactly equal to a value,
-- untilM is bypassed and that value is assigned directly.
--
-- e.g. constrain tcpDstPort :: Bits16 16 <| tcpDstPort == 22 |> will avoid
-- creating new 16-bit values until one happens to be equal to 22, and will
-- instead assign the literal 22 to tcpDstPort.
genGenConstrain :: Pat -> PadsTy -> Exp -> Q Exp
genGenConstrain pat pty e = do
  name <- newName "x"
  orig <- bindS (varP name) (genGenTy pty)
  let pat' = return pat; e' = return e
  let (VarE x) = fromVarP pat; xName = nameBase x
  gen <- case e of
    (UInfixE
      (VarE (Name (OccName var)  NameS))
      (VarE (Name (OccName "==") NameS))
      y) | var == xName -> bindS pat' [| return $(return y) |]
    (UInfixE
      y
      (VarE (Name (OccName "==") NameS))
      (VarE (Name (OccName var)  NameS)))
        | var == xName -> bindS pat' [| return $(return y) |]
    _ -> bindS pat' [| untilM $(lamE [pat'] e') (const $(genGenTy pty)) recLimit $(varE name) |]
  ret  <- noBindS [| return $(return $ fromVarP pat) |]
  return $ DoE [orig,gen,ret]
  where
    fromVarP :: Pat -> Exp
    fromVarP (VarP x) = VarE x

-- | If an optional generator is included in the quasiquoted PADS description,
-- simply provide it. If not, fail with a (hopefully) helpful error message.
genGenTransform :: PadsTy -> PadsTy -> Exp -> Maybe Exp -> Q Exp
genGenTransform src dest exp genM = case genM of
  Just g  -> return g
  Nothing -> [| (return $
                 error  $ "genGenTy: PTransform unimplemented. You likely arrived "
                       ++ "at this error by having an \"obtain\" declaration/expression "
                       ++ "in your description. If so, you can provide your own "
                       ++ "generation function f by appending \" generator f\" "
                       ++ "to it.") :: PadsGen a |]

-- | Generate a list representing a Pads list type. We ignore the separator and
-- PadsTy termination condition here and include them in the data during
-- serialization.
genGenList :: PadsTy -> (Maybe PadsTy) -> (Maybe TermCond) -> Q Exp
genGenList pty sep term =
  case (sep,term) of
    (_, Just (LLen l)) -> [| sequence $ replicate $(return l) $(genGenTy pty) |]
    _ -> do
      name <- newName "n"
      bind <- bindS (varP name) [| randNumBound generatedListLengthLimit |]
      ret  <- noBindS [| sequence $ replicate $(varE name) $(genGenTy pty) |]
      return $ DoE (bind : [ret])
  where
    generatedListLengthLimit = 100

-- | All variables on which a PValue statement depends will be in scope at this
-- point, so the expression can be returned and evaluated at runtime.
genGenValue :: Exp -> Q Exp
genGenValue exp = [| return $(return exp) |]

-- | Generate the generator for a Pads tuple
genGenTuple :: [PadsTy] -> Q Exp
genGenTuple [] = [| return () |]
genGenTuple tys = do
  tys'  <- mapM genGenTy (filter hasRep tys)
  names <- sequence [newName "x" | t <- tys']
  let stmts = [BindS (VarP n) t | (n,t) <- zip names tys']
  ret <- noBindS [| return $(tupE (map varE names)) |]
  return $ DoE (stmts ++ [ret])

-- | Generate the generator for a Pads type application.
genGenTyApp :: [PadsTy] -> Maybe Exp -> Q Exp
genGenTyApp tys expM = do
  tys' <- mapM genGenTy tys
  return (foldl1 AppE (tys' ++ Maybe.maybeToList expM))

-- | Basically same as mkParseTycon, but the name that results is different.
mkGenTycon :: QString -> Q Exp
mkGenTycon ["EOF"] = varE 'eOF_genM
mkGenTycon ["EOR"] = varE 'eOR_genM
mkGenTycon c = (varE . mkTyGeneratorQName) c

-- | Basically same as mkParseTyvar, but the name that results is different.
mkGenTyvar :: String -> Q Exp
mkGenTyvar v = varE (mkVarGeneratorName v)


-------------------------------------------------------------------------------
-- * Generating Serialization Functions

-- | Create the serializer for a PadsDeclType declaration
genPadsSerialize :: UString -> [LString] -> Maybe Pat -> PadsTy -> Q [Dec]
genPadsSerialize name args patM padsTy = do
  body <- genSerializeTy padsTy ((Just . VarE . mkName) "rep")
  return [mkSerializerFunction name args patM body]

-- | Create the serializer for a PadsDeclData declaration
genPadsDataSerialize :: UString -> [LString] -> Maybe Pat -> PadsData -> Q [Dec]
genPadsDataSerialize name args patM padsData = do
  body <- genSerializeData padsData ((Just . VarE . mkName) "rep")
  return [mkSerializerFunction name args patM body]

-- | Create the serializer for a PadsDeclNew declaration
genPadsNewSerialize :: UString -> [LString] -> Maybe Pat -> BranchInfo -> Q [Dec]
genPadsNewSerialize name args pat branch = do
  exp <- genSerializeUnion [branch] ((Just . VarE . mkName) "rep")
  return [mkSerializerFunction name args pat exp]

-- | Create the serializer for a PadsDeclObtain declaration
genPadsObtainSerialize :: UString -> [LString] -> PadsTy -> Exp -> Q [Dec]
genPadsObtainSerialize name args padsTy exp = do
  let trans = PTransform padsTy (PTycon [name]) exp Nothing
  e <- genSerializeTy trans ((Just . VarE . mkName) "rep")
  return [mkSerializerFunction name args Nothing e]

-- | Create the function declaration for a serialization function
mkSerializerFunction :: UString -> [LString] -> Maybe Pat -> Exp -> Dec
mkSerializerFunction name args patM body =
  FunD serializerName [Clause (serializerArgs ++ [(VarP . mkName) "rep"]) (NormalB body) []]
  where
  serializerName = mkTySerializerName name
  serializerArgs = map (VarP . mkTySerializerVarName) args ++ Maybe.maybeToList patM

-- | Create the serializer for a given form of PadsData
genSerializeData :: PadsData -> Maybe Exp -> Q Exp
genSerializeData (PUnion bs) rep = genSerializeUnion bs rep
genSerializeData (PSwitch exp pbs) rep = genSerializeSwitch exp pbs rep

-- | Create the serializer for a PUnion type of data constructor
genSerializeUnion :: [BranchInfo] -> Maybe Exp -> Q Exp
genSerializeUnion bs (Just rep) = do
  matches <- concat <$> mapM genSerializeBranchInfo bs
  return $ CaseE rep matches
genSerializeUnion bs Nothing = error "genSerializeUnion: expected rep"

-- | At the serialization stage, a PSwitch is simply a sugared PUnion; treat it
-- accordingly here.
genSerializeSwitch :: Exp -> [(Pat,BranchInfo)] -> Maybe Exp -> Q Exp
genSerializeSwitch _ pbs r = genSerializeUnion (map snd pbs) r

-- | Dispatch to the appropriate function based on the type of BranchInfo.
genSerializeBranchInfo :: BranchInfo -> Q [Match]
genSerializeBranchInfo (BRecord c fields predM) = genSerializeRecord c fields predM
genSerializeBranchInfo (BConstr c args predM) = genSerializeConstr c args predM

-- | Serialization of records is accomplished with a case statement at runtime
-- to bring all names of variables into scope
genSerializeRecord :: UString -> [FieldInfo] -> Maybe Exp -> Q [Match]
genSerializeRecord recName fields predM = do
  let (namesM, tys) = unzip (map (\(n,(_,t),_,_) -> (n,t)) fields)
  let serializers = map (\(n,t) -> genSerializeTy t ((VarE . mkName) <$> n)) (zip namesM tys)
  let serialized = [app s n t | (s,n,t) <- zip3 serializers namesM tys]
  casePat  <- conP (mkName recName) (map (varP . mkName) (Maybe.catMaybes namesM))
  caseBody <- normalB [| concatCs $(listE serialized) |]
  return [Match casePat caseBody []]
  where
    app :: Q Exp -> Maybe String -> PadsTy -> Q Exp
    app s (Just n) t = s
    app s Nothing  t = if hasRep t then s `appE` (genDefTy t) else s

-- | Serialization of branch constructors is somewhat similar to that of
-- records, but differs in the lack of named variables. Simply create TH
-- newNames for each relevant variable or constant.
genSerializeConstr :: String -> [ConstrArg] -> Maybe Exp -> Q [Match]
genSerializeConstr name args predM = do
  let tys = [ty | (_,ty) <- args]
  let tys' = map (flip genSerializeTy Nothing) tys
  names <- sequence [newName "x" | ty <- tys]
  let params = [varE n | n <- names]
  let apps = listE [if hasRep t then s `appE` p else s | (s,p,t) <- zip3 tys' params tys]
  matchPat  <- conP (mkName name) [varP n | (n, t) <- zip names tys, hasRep t]
  matchBody <- normalB $ [| concatCs $apps |]
  return [Match matchPat matchBody []]

-- | Driver function to serialize PadsTys, dispatches to the appropriate helper.
-- The "Maybe Exp" parameter informs a function whether or not it needs to
-- apply the serializer it creates to the variable standing for the Haskell data
-- representation - usually "rep" in generated code.
genSerializeTy :: PadsTy -> (Maybe Exp) -> Q Exp
genSerializeTy (PConstrain pat ty exp) r     = genSerializeConstrain pat ty exp r
genSerializeTy (PTransform src dest exp _) r = genSerializeTransform src dest exp r
genSerializeTy (PList ty sepM termM) r       = genSerializeList ty sepM termM r
genSerializeTy (PPartition ty exp) r         = genSerializePartition ty exp r
genSerializeTy (PValue exp ty) r             = genSerializeValue exp ty r
genSerializeTy (PApp tys expM) r             = genSerializeApp tys expM r
genSerializeTy (PTuple tys) r                = genSerializeTuple tys r
genSerializeTy (PExpression exp) r           = genSerializeExp exp r
genSerializeTy (PTycon c) r                  = genSerializeTycon c r
genSerializeTy (PTyvar v) r                  = genSerializeTyvar v r

-- | At the serialization stage, already existing data cannot be constrained,
-- unlike in the generation stage. Here we merely pass the type back into
-- genSerializeTy to obtain its serializer.
genSerializeConstrain :: Pat -> PadsTy -> Exp -> (Maybe Exp) -> Q Exp
genSerializeConstrain _ ty _ r = genSerializeTy ty r

-- | Serialization of a PTransform PadsTy requires only a thin skin atop the
-- functions provided for converting between types.
genSerializeTransform :: PadsTy -> PadsTy -> Exp -> (Maybe Exp) -> Q Exp
genSerializeTransform src dest (TupE [srcToDest,destToSrc]) r = do
  let srcSerializer = genSerializeTy src Nothing
  let destToSrc' = [| \x -> $(return destToSrc) (x, undefined) |]
  let serializer = [| $srcSerializer . fst . $destToSrc' |]
  case r of
    Just rep -> [| $serializer $(return rep) |]
    Nothing  -> [| $serializer               |]

-- | Create a serializer for a PList, which will intersperse separators and
-- incorporate terminating conditions as necessary.
genSerializeList :: PadsTy -> (Maybe PadsTy) -> (Maybe TermCond) -> (Maybe Exp) -> Q Exp
genSerializeList ty sepM termM r = do
  let s = genSerializeTy ty Nothing
  cs   <- newName "cs"
  cs'  <- newName "cs_sep"
  cs'' <- newName "cs_sep_term"
  dec1 <- [d| $(varP cs) = map $s $(dyn "rep") |]
  dec2 <- case sepM of
    Nothing -> [d| $(varP cs') = $(varE cs) |]
    Just s  -> let
      def   = genDefTy s
      def_s = genSerializeTy s Nothing
      app = if hasRep s then def_s `appE` def else def_s
      in  [d| $(varP cs') = intersperse $app $(varE cs) |]
  dec3 <- case termM of
    Nothing        -> [d| $(varP cs'') = concatCs $(varE cs') |]
    Just (LLen e)  -> [d| $(varP cs'') = concatCs $(varE cs') |]
    Just (LTerm t) -> let
      def   = genDefTy t
      def_s = genSerializeTy t Nothing
      app = if hasRep t then def_s `appE` def else def_s
      in  [d| $(varP cs'') = concatCs $(varE cs') `cApp` $app |]
  let lamArgs = [(VarP . mkName) "rep"]
  let letDecs = dec1 ++ dec2 ++ dec3
  return $
    case r of Just rep -> (LamE lamArgs $ LetE letDecs (VarE cs'')) `AppE` rep
              Nothing  -> (LamE lamArgs $ LetE letDecs (VarE cs''))

-- | Create a serializer for a PPartition type. We can ignore "bytes X" and
-- "none" disciplines, as such disciplines are only relevant to parsing, and
-- simply serialize the underlying type. As for "newline" and "windows"
-- disciplines, instead of figure out where to place the relevant characters,
-- provide a helpful error.
genSerializePartition :: PadsTy -> Exp -> (Maybe Exp) -> Q Exp
genSerializePartition ty exp r
  | exp == (VarE (Name (OccName "newline") NameS))
        = [| error "genSerializePartition: unimplemented: newline discipline" |]
  | exp == (VarE (Name (OccName "windows") NameS))
        = [| error "genSerializePartition: unimplemented: windows discipline" |]
  | otherwise = genSerializeTy ty r

-- | PValues are stored in a parse result but do not appear in the original
-- data. Relying on all serializations being concatenated, where each
-- serialization is a CList, we can provide an "empty" serialization for a
-- PValue with (const) id.
genSerializeValue :: Exp -> PadsTy -> (Maybe Exp) -> Q Exp
genSerializeValue _ _ (Just rep) = [|       id |]
genSerializeValue _ _ Nothing    = [| const id |]

-- | A PADS application of types is translated directly to a Template Haskell
-- application (AppE).
genSerializeApp :: [PadsTy] -> (Maybe Exp) -> (Maybe Exp) -> Q Exp
genSerializeApp tys expM r = do
  serializers <- mapM (flip genSerializeTy Nothing) tys
  return (foldl1 AppE (serializers ++ Maybe.maybeToList expM ++ Maybe.maybeToList r))

-- | In the runtime function, a case statement is deployed to ensure the input
-- has the correct tuple format, then a serializer for each element of the tuple
-- is bound in a let statement, with their results concatenated to create the
-- function's overall result.
genSerializeTuple :: [PadsTy] -> (Maybe Exp) -> Q Exp
genSerializeTuple tys r = do
  let serializers = map (flip genSerializeTy Nothing) tys
  letnames  <- sequence [newName "k" | s <- serializers] -- newName "x" results in a capturable name?
  casenames <- sequence [newName "y" | s <- serializers]
  let letdecs = map mkDec (zip3 letnames casenames (zip tys serializers))
  let letbody = [| concatCs $(listE $ map varE letnames) |]
  let casebody = normalB $ letE letdecs letbody
  let casenames' = [cn | (cn,ty) <- zip casenames tys, hasRep ty]
  case r
    of Just rep -> let
         lamArgs = [(varP . mkName) "rep"]
         matches = [match (tupP [varP cn | cn <- casenames']) casebody []]
         in (lamE lamArgs (caseE (dyn "rep") matches)) `appE` (return rep)
       Nothing -> let
         lamArgs = [(varP . mkName) "rep"]
         matches = [match (tupP [varP cn | cn <- casenames']) casebody []]
         in (lamE lamArgs (caseE (dyn "rep") matches))
  where
    mkDec :: (Name, Name, (PadsTy, Q Exp)) -> Q Dec
    mkDec (ln, cn, (t, t')) = if hasRep t
      then valD (varP ln) (normalB (appE t' (varE cn))) []
      else valD (varP ln) (normalB       t'           ) []


-- | The runtime function exp_serialize can be called on literal numbers,
-- characters, and strings, and will serialize them appropriately.
genSerializeExp :: Exp -> (Maybe Exp) -> Q Exp
genSerializeExp exp _ = [| exp_serialize $(return exp) |] --error "genSerializeExp: unexpected representation" --appE [| exp_serialize $(return exp) |] (return rep)

-- | A PTycon is represented according to mkTySerializerName, where the
-- resultant name will be an in-scope runtime serializer.
genSerializeTycon :: QString -> (Maybe Exp) -> Q Exp
genSerializeTycon c r = case r of
  (Just rep) -> return $ AppE ((VarE . mkTySerializerQName) c) rep
  Nothing    -> return $       (VarE . mkTySerializerQName) c

-- | A PTyvar is represented according to mkTySerializerVarName, where the
-- resultant name will stand for a serializer the user must provide.
genSerializeTyvar :: String -> (Maybe Exp) -> Q Exp
genSerializeTyvar s (Just rep) = return $ (VarE $ mkTySerializerVarName s) `AppE` rep
genSerializeTyvar s Nothing    = return $ (VarE $ mkTySerializerVarName s)

-------------------------------------------------------------------------------
-- Generating Printing Function from a Declaration

-- | Generate the lazy "function list" printer for a given 'PadsTy' Pads type as
-- parsed using Pads' plain-type syntactic form..
genPadsPrintFL :: UString -> [LString] -> Maybe Pat -> PadsTy -> Q [Dec]
genPadsPrintFL name args patM padsTy = do
  let rm = [mkName "rep", mkName "md"]
  body  <- genPrintTy padsTy $ Just $ TupE (map VarE rm)
  return [mkPrinterFunction name args rm patM body]

-- | Generate the lazy function list printer for the Pads data-type syntactic
-- form.
genPadsDataPrintFL :: UString -> [LString] -> Maybe Pat -> PadsData -> Q [Dec]
genPadsDataPrintFL name args patM padsData = do
  let rm = [mkName "rep", mkName "md"]
  body  <- genPrintData padsData $ Just $ TupE (map VarE rm)
  return [mkPrinterFunction name args rm patM body]

-- | Generate the lazy function list printer for the Pads newtype syntactic form.
genPadsNewPrintFL :: UString -> [LString] -> Maybe Pat -> BranchInfo -> Q [Dec]
genPadsNewPrintFL name args patM branch = do
  let rm = [mkName "rep", mkName "md"]
  matches <- genPrintBranchInfo False branch
  let body = CaseE (TupE (map VarE rm)) matches
  return [mkPrinterFunction name args rm patM body]

-- | Generate the lazy function list printer for the Pads obtain syntactic form.
genPadsObtainPrintFL :: UString -> [LString] -> PadsTy -> Exp -> Q [Dec]
genPadsObtainPrintFL name args padsTy exp = do
  let rm = [mkName "rep", mkName "md"]
  body  <- genPrintTy (PTransform padsTy (PTycon [name]) exp Nothing) $ Just $ TupE (map VarE rm)
  return [mkPrinterFunction name args rm Nothing body]

-- | Make the function declaration for the "lazy function list" printer with the
-- body as generated by 'genPrintTy', 'genPrintData', or 'genPrintBranchInfo' as
-- passed into this function as the last 'Exp' parameter.
mkPrinterFunction :: UString -> [LString] -> [Name] -> Maybe Pat -> Exp -> Dec
mkPrinterFunction name args rm patM body =
  FunD printerName [Clause (printerArgs ++ [TupP (map VarP rm)]) (NormalB body) []]
  where
  printerName = mkTyPrinterName name
  printerArgs = map (VarP . mkTyPrinterVarName) args ++ Maybe.maybeToList patM

-------------------------------------------------------------------------------
-- * Generate Printing Function from a Type

-- | Generate the body of the printing function for a Pads type - this function
-- dispatches to the ones below according to the syntactic form being
-- translated.
genPrintTy :: PadsTy -> Maybe Exp -> Q Exp
genPrintTy (PConstrain pat ty exp) rm   = genPrintTy ty rm  -- XXX: doesn't check the constraint; ideally we should change @printFL@ to account for possible printing errors
genPrintTy (PTransform src dest exp _) rm = genPrintTrans src exp rm
genPrintTy (PList ty sepM termM) rm     = genPrintList ty sepM termM >>= applyPrintTy rm
genPrintTy (PPartition ty exp) rm       = [| (error "genPrintTy PPartition not implemented") |] --genPrintPartition ty exp rm
genPrintTy (PApp tys expM) rm           = genPrintTyApp tys expM >>= applyPrintTy rm
genPrintTy (PTuple tys) rm              = genPrintTuple tys rm
genPrintTy (PExpression exp) rm         = genPrintExp exp rm
genPrintTy (PTycon c) rm                = genPrintTycon c >>= applyPrintTy rm
genPrintTy (PTyvar v) rm                = genPrintTyVar v >>= applyPrintTy rm
genPrintTy (PValue exp ty) rm           = genPrintValue exp rm

-- | Generate the printer for the Pads Value syntactic form 'PValue'. Because a
-- pads value is something that wasn't parsed (it's a way to compute / add an extra
-- field to a parsed Haskell record), we just return the 'nil' printer (prints
-- nothing).
genPrintValue :: Exp -> Maybe Exp -> Q Exp
genPrintValue exp rm = return $ VarE 'nil

-- | Generate the printer for the Pads Transform syntactic form 'PTransform'.
-- This means we need to grab the second function from the tuple provided by the
-- Pads programmer which corresponds to the inverse of the transform function,
-- and print the format of the resulting (source) type. Source here means what's
-- read from a file and destination type means the type for which we have a
-- value that we want to print out. In order for round-trip parsing to work, we
-- need to reverse the transformation because the on-disk format of the source
-- type is usually different from the on-disk format of the destination type.
genPrintTrans :: PadsTy -> Exp -> Maybe Exp -> Q Exp
genPrintTrans tySrc exp Nothing
  = genPrintTy tySrc Nothing
genPrintTrans tySrc (TupE [_, fncn]) (Just rm) = do
  rm' <- [| $(return fncn) $(return rm) |]
  genPrintTy tySrc (Just rm')
genPrintTrans _ tup _ = error ("Template Haskell exp '" ++ show tup ++ "' does not appear to be a two-tuple.")

-- | Some of the printing utilities provided by the runtime system need to know
-- about the representation and the metadata. If the first argument to this
-- function is Nothing, then we don't need to pass the representation and
-- metadata to the expression / utility (e.g. ca case expression printing a
-- union type). Otherwise the first argument contains 'Just' the '(rep, md)'
-- tuple brought into scope as the first parameter to the "*_printFL" functions
-- (e.g. the 'printList' runtime system function needs to know about the rep and
-- md).
applyPrintTy :: Maybe Exp -> Exp -> Q Exp
applyPrintTy rm f = do
  case rm of
    Nothing -> return f
    Just repmdE -> return $ AppE f repmdE

-- | Generate the template haskell code for printing a 'PList' Pads type.
genPrintList :: PadsTy -> Maybe PadsTy -> Maybe TermCond -> Q Exp
genPrintList ty sepOpt termCondOpt = do
  (elemRepE, elemRepP) <- doGenPE "elemrep"
  (elemMDE,  elemMDP)  <- doGenPE "elemmd"
  parseElemE <- genPrintTy ty $ Just $ TupE [elemRepE,elemMDE]
  let parseElemFnE = LamE [TupP [elemRepP, elemMDP]] parseElemE
  sepElemE <- case sepOpt of
    Nothing -> return (VarE 'printNothing)
    Just ty -> do
      def <- genDefTy ty
      genPrintTy ty $ Just $ TupE [SigE def (mkRepTy ty),SigE (VarE 'myempty) (mkMDTy False ty)]
  termElemE <- case termCondOpt of
    Nothing -> return (VarE 'printNothing)
    Just (LLen _) -> return (VarE 'printNothing)
    Just (LTerm (PApp [PTycon ["Try"],_] _)) -> return (VarE 'printNothing)
    Just (LTerm (PTuple [PApp [PTycon ["Try"],_] _])) -> return (VarE 'printNothing)
    Just (LTerm termTy) -> do
      def <- genDefTy termTy
      genPrintTy termTy $ Just $ TupE [SigE def (mkRepTy termTy),SigE (VarE 'myempty) (mkMDTy False termTy)]
  return $ appE3 (VarE 'printList) parseElemFnE sepElemE termElemE

-- | Generate the template haskell code for printing a Pads type application by
-- recursively calling 'genPrintTy' on the Pads types of each of the arguments to the
-- Pads type constructor.
genPrintTyApp :: [PadsTy] -> Maybe Exp -> Q Exp
genPrintTyApp tys expM = do
  prtys <- mapM (flip genPrintTy Nothing) tys
  foldl1M (\e1 e2 -> return $ AppE e1 e2) (prtys ++ Maybe.maybeToList expM)

-- | Generate the template haskell code for printing a Pads tuple type.
genPrintTuple :: [PadsTy] -> Maybe Exp -> Q Exp
genPrintTuple tys (Just rm) = do
  repNamesM <- genNamesforTuple True "rep" tys
  let repVars = map VarE (Maybe.catMaybes repNamesM)
  let repPats = map VarP (Maybe.catMaybes repNamesM)
  mdNamesM  <- genNamesforTuple False "md" tys
  let mdVars = map VarE (Maybe.catMaybes mdNamesM)
  let mdPats = map VarP (Maybe.catMaybes mdNamesM)
  inners <- sequence [genPrintTupleInner t r m | (t,r,m) <- zip3 tys repNamesM mdNamesM{-, hasRep t-}]
  return $ CaseE rm
                [Match (TupP [TupP $ repPats, TupP [SigP WildP (ConT ''Base_md), (TupP mdPats)]])
                       (NormalB (VarE 'concatFL `AppE` ListE inners))
                       []]
genPrintTuple tys Nothing = do
  repName <- newName "rep"
  mdName <- newName "md"
  liftM (LamE [TupP [VarP repName,VarP mdName]]) $ genPrintTuple tys $ Just $ TupE [VarE repName,VarE mdName]

-- | Filters a second list based on which corresponding Pads types from the
-- first list have an underlying representation in memory (removing the ones
-- that don't have an underlying representation).
filterByHasRep :: [PadsTy] -> [a] -> [a]
filterByHasRep tys xs = map snd $ filter (hasRep . fst) (zip tys xs)

-- | Generate a list of names to be used as Haskell pattern variables and
-- expression variables for a Pads tuple type. If the tuple is for the
-- representation then the given 'Bool' is True and we want to ignore data that
-- doesn't have a representation in memory. Otherwise the tuple is for the
-- metadata meaning the given 'Bool' is False and we want to print *everything*.
genNamesforTuple :: Bool -> String -> [PadsTy] -> Q [Maybe Name]
genNamesforTuple False str tys = sequence [fmap Just (newName str) | ty <- tys]
genNamesforTuple True str tys = sequence [if hasRep ty then fmap Just (newName str) else return Nothing | ty <- tys]

-- | Generate the template haskell print function for some type inside of a
-- tuple based on whether or not that type has an in-memory representation
-- '(Just r)' and a metadata representation '(Just m)'.
genPrintTupleInner t (Just r) (Just m)  = genPrintTy t (Just (TupE [VarE r,VarE m]))
genPrintTupleInner t Nothing (Just m)   = genDefTy t >>= \def -> genPrintTy t (Just (TupE [def, VarE m]))
genPrintTupleInner t Nothing Nothing    = genPrintTy t Nothing
genPrintTupleInner t (Just r) Nothing   = error ("genPrintTupleInner: Type '" ++ show t
  ++ "' has a representation but no metadata.")

-- | Generate the template haskell code for printing the value of a Pads literal
-- (string, character, regex) by simply constructing a runtime system call to
-- 'litPrint' with the code for computing the Haskell value of the literal
-- spliced into the first argument position.
genPrintExp :: Exp -> Maybe Exp -> Q Exp
genPrintExp e _ = [| litPrint $(return e) |]

-- | Generate the printer for a Pads type constructor (hint: it's just the
-- variable name according to 'mkTyPrinterQName'.
genPrintTycon :: QString -> Q Exp
genPrintTycon c = return $ VarE (mkTyPrinterQName c)

-- | Generate the printing expression for a Pads type variable according to
-- 'mkTyPrinterVarName'.
genPrintTyVar :: LString -> Q Exp
genPrintTyVar v = return $ VarE (mkTyPrinterVarName v)

-------------------------------------------------------------------------------
-- Generate Printing Function from a Datatype

-- | Generate the template haskell expression for printing a Haskell value given
-- the Pads data type declaration defining the type of the Haskell value.
genPrintData :: PadsData -> Maybe Exp -> Q Exp
genPrintData (PUnion bs) rm = genPrintUnion bs rm
genPrintData (PSwitch exp pbs) rm = genPrintSwitch exp pbs rm

-- | Generate a Haskell case expression for printing a Pads union type.
genPrintUnion :: [BranchInfo] -> Maybe Exp -> Q Exp
genPrintUnion bs (Just rm) = do
  let doDef = if length bs > 1 then True else False
  matches <- liftM concat $ mapM (genPrintBranchInfo doDef) bs
  return $ CaseE rm matches
genPrintUnion bs Nothing = do
  repName <- newName "rep"
  mdName <- newName "md"
  let doDef = if length bs > 1 then True else False
  matches <- liftM concat $ mapM (genPrintBranchInfo doDef) bs
  return $ LamE [TupP [VarP repName,VarP mdName]] $ CaseE (TupE [VarE repName,VarE mdName]) matches

-- | Generate the printing function body of an individual branch of a Pads data type.
genPrintBranchInfo :: Bool -> BranchInfo -> Q [Match]
genPrintBranchInfo doDef (BRecord c fields predM) =  genPrintRecord c fields predM
genPrintBranchInfo doDef (BConstr c args predM) = genPrintConstr doDef c args predM

-- | Generate the individual 'Match' of the Haskell case expression for matching
-- on a record being printed.
genPrintRecord :: UString -> [FieldInfo] -> Maybe Exp -> Q [Match]
genPrintRecord (mkName -> recName) fields predM = do
  (repEs, repPs) <- getPEforFields (\t -> genDefTy t >>= \def -> return $ SigE def (mkRepTy t)) (return . getBranchNameL) fields
  (mdEs,  mdPs)  <- getPEforFields (return . SigE (VarE 'myempty) . mkMDTy False) (return . getBranchMDNameL) fields
  let ptys = map (\(n,(_,ty),p,_) -> ty) fields
  let ty_rep_mds = zip3 ptys repEs mdEs
  expE <- mapM (\(ty,r,m) -> genPrintTy ty $ Just $ TupE [r,m]) ty_rep_mds
  let printItemsE = ListE expE
  let caseBody = NormalB (AppE (VarE 'concatFL) printItemsE)
  let mdPat  = TupP[WildP, RecP (getStructInnerMDName recName) mdPs]
  let repPat = RecP recName repPs
  let casePat = TupP [repPat, mdPat]
  let match = Match casePat caseBody []
  return [match]

-- | Get the printer expression for an individual field of a record.
getPEforField :: (PadsTy -> Q Exp) -> (String -> Q Name) -> FieldInfo -> Q (Exp, Maybe FieldPat)
getPEforField def mkFieldNm (nameOpt, (strict,pty), optPred, _) = case nameOpt of
  Nothing -> def pty >>= \d -> return (d,Nothing)
  Just str -> do
    name <- mkFieldNm str
    let (varE, varP) = genPE name
    return (varE, Just (name, varP))

-- | Get the printer expressions and corresponding record field pattern
-- matches for each of the given 'FieldInfo's.
getPEforFields :: (PadsTy -> Q Exp) -> (String -> Q Name) -> [FieldInfo] -> Q ([Exp], [FieldPat])
getPEforFields def mkFieldNm fields = do
  eps <- mapM (getPEforField def mkFieldNm) fields
  let (es, pOpts) = List.unzip eps
      ps = Maybe.catMaybes pOpts
  return (es, ps)

-- | Generate the template haskell code for matching on and printing the value
-- for a Pads value constructor.
genPrintConstr :: Bool -> String -> [ConstrArg] -> (Maybe Exp) -> Q [Match]
genPrintConstr doDef (mkName -> recName) args predM = do
  let fields = map (\c -> (Just "arg",c,Nothing,Nothing)) args
  (repEs, repPs) <- getPEforFields (\t -> genDefTy t >>= \def -> return $ SigE def (mkRepTy t)) newName fields
  (mdEs,  mdPs)  <- getPEforFields (return . SigE (VarE 'myempty) . mkMDTy False) newName fields
  let ptys = map (\(n,(s,ty),p,_) -> ty) fields

  let genBody mdEs = (do
      { let genTyRepMd = (\(ty,r,m) -> if hasRep ty then return (ty,r,m) else genDefTy ty >>= (\def -> return (ty,SigE def (mkRepTy ty),m)))
      ; ty_rep_mds <- mapM genTyRepMd $ zip3 ptys repEs mdEs
      ; expE <- mapM (\(ty,repE,mdE) -> genPrintTy ty $ Just $ TupE [repE,mdE]) ty_rep_mds
      ; let printItemsE = ListE expE
      ; let caseBody = NormalB (AppE (VarE 'concatFL) printItemsE)
      ; return caseBody
      })

  let repPat = ConP recName (filterByHasRep ptys $ map snd repPs)
  let mdPat  = TupP[SigP WildP (ConT ''Base_md), ConP (getStructInnerMDName recName) (map snd mdPs)]

  caseBody <- genBody mdEs
  let match = Match (TupP [repPat, mdPat]) caseBody []

  caseBodyDef <- genBody $ map (\(_,ty) -> SigE (VarE 'myempty) (mkMDTy False ty)) args
  let matchDef = Match (TupP [repPat,WildP]) caseBodyDef []
  if doDef then return [match,matchDef] else return [match]

-- | Generate the template haskell code for printing a Pads switch type by
-- ignoring the value we're switching on and simply generating the same case
-- expression that 'genPrintUnion' does for a Pads union type.
genPrintSwitch :: Exp -> [(Pat,BranchInfo)] -> Maybe Exp -> Q Exp
genPrintSwitch exp pbs rm = genPrintUnion (map snd pbs) rm

-------------------------------------------------------------------------------
-- * Generating Default Function from a Declaration

-- | Generate the Pads default value for a 'PadsDeclType'
genPadsDef :: UString -> [LString] -> Maybe Pat -> PadsTy -> Q [Dec]
genPadsDef name args patM padsTy = do
  body  <- genDefTy padsTy
  return [mkDefFunction name args patM body]

-- | Generate the Pads default value for a Pads data declaration.
genPadsDataDef :: UString -> [LString] -> Maybe Pat -> PadsData -> Q [Dec]
genPadsDataDef name args patM padsData = do
  body  <- genDefData padsData
  return [mkDefFunction name args patM body]

-- | Generate the Pads default value for a Pads newtype declaration.
genPadsNewDef :: UString -> [LString] -> Maybe Pat -> BranchInfo -> Q [Dec]
genPadsNewDef name args patM branch = do
  body <- genDefBranchInfo branch
  return [mkDefFunction name args patM body]

-- | Generate the Pads default value for a Pads obtain declaration.
genPadsObtainDef :: UString -> [LString] -> PadsTy -> Exp -> Q [Dec]
genPadsObtainDef name args padsTy exp = do
  body  <- genDefTy (PTransform padsTy (PTycon [name]) exp Nothing)
  return [mkDefFunction name args Nothing body]

-- | Generate the Pads default value as a function declaration of the form
-- "foo_def" for a Pads parser named "Foo".
mkDefFunction :: UString -> [LString] -> Maybe Pat -> Exp -> Dec
mkDefFunction name args patM body =
  FunD defName [Clause (defArgs) (NormalB body) []]
  where
  defName = mkTyDefName name
  defArgs = map (VarP . mkTyDefVarName) args ++ Maybe.maybeToList patM

-------------------------------------------------------------------------------
-- * Generate Default Function from a Type

-- | Generate the default Haskell value for some Pads type.
genDefTy :: PadsTy -> Q Exp
genDefTy (PConstrain pat ty exp)   = genDefTy ty  -- XXX: doesn't check the constraint; ideally we should change @printFL@ to account for possible printing errors
genDefTy (PTransform src dest exp _) = do
  defSrc <- genDefTy src
  srcToDest <- [| \rep -> fst $ (fst $(return exp)) S.zeroSpan (rep,(error "TODO defaultMd")) |] -- XXX: fix this undefined, it kind of requires defaultMd to be defined inductively over Pads types as well...
  return $ AppE srcToDest defSrc
genDefTy (PList ty sepM termM)     = [| [] |]
genDefTy (PPartition ty exp)       = genDefTy ty
genDefTy (PApp tys expM)           = do
  prtys <- mapM genDefTy tys
  foldl1M (\e1 e2 -> return $ AppE e1 e2) (prtys ++ Maybe.maybeToList expM)
genDefTy (PTuple tys)              = genDefTuple tys
genDefTy (PExpression exp)         = return exp
genDefTy (PTycon c)                = return $ VarE (mkTyDefQName c)
genDefTy (PTyvar v)                = return $ VarE (mkTyDefVarName v)
genDefTy (PValue exp ty)           = genDefTy ty

-- | Generate the default Haskell value for a Pads tuple type.
genDefTuple :: [PadsTy] -> Q Exp
genDefTuple tys = case reps of
  [] -> [| () |]
  [ty] -> genDefTy ty
  tys -> do
    exps <- mapM genDefTy tys
    return $ TupE exps
  where
  reps = [ty | ty <- tys, hasRep ty]

-------------------------------------------------------------------------------
-- Generate Default Function from a Datatype

-- | Generate the default Haskell value for a Pads data type 'PadsData'.
genDefData :: PadsData -> Q Exp
genDefData (PUnion (b:bs))        = genDefBranchInfo b
genDefData (PSwitch exp (pb:pbs)) = genDefBranchInfo (snd pb)
genDefData (PUnion [])            = error "genDefData: empty PUnion."
genDefData (PSwitch exp [])       = error "genDefData: empty PSwitch."

-- | Generate the default Haskell value for a single branch of a Pads type,
-- namely either a Pads constructor or record.
genDefBranchInfo :: BranchInfo -> Q Exp
genDefBranchInfo (BConstr c args pred) = do
  reps <- sequence $ [genDefTy ty | (strict,ty) <- args, hasRep ty]
  return $ foldl1 AppE (ConE (mkConstrName c):reps)
genDefBranchInfo (BRecord c fields expM) = do
  reps <- sequence $ [liftM (l,) (genDefTy ty) | (Just l,(strict,ty),_,_) <- fields, hasRep ty]

  let lets = flip map reps $ \(lab,def) -> ValD (VarP $ mkName lab) (NormalB def) []
  return $ LetE lets $ foldl1 AppE (ConE (mkConstrName c):map (VarE . mkName . fst) reps)

-------------------------------------------------------------------------------
-- * Name Manipulation Functions

-- ** Naming types, and accessing the names of types

-- | Get the template haskell 'Name' for a given Pads type.
mkRepName :: String -> Name
mkRepName str = mkName str

-- | Make the template haskell 'Name' of a given 'PTycon' with a qualified name.
mkRepQName :: QString -> Name
mkRepQName str = mkName (qName str)

-- | Make externally visible metadata name for a Pads type
mkMDName :: String -> Name
mkMDName str = mkName (str ++ "_md")

-- | Given a Pads type name in the template haskell @Q@ monad, get the metadata
-- type name.
mkMDQName :: QString -> Name
mkMDQName str = mkName (appendTo str "_md")

-- | Make the internal metadata type name for a given Pads type
mkIMDName name  = mkName (name ++ "_imd")

-- | Make externally visible metadata name for a Pads variable
mkMDVarName name = mkName (name ++ "_md")

-- ** Naming fields and constructors

-- | Convert Pads source (record) field name into a 'Q' monad name
mkFieldName str   = mkName str

-- | Convert Pads source (record) field name into its metadata name in the 'Q'
-- monad.
mkFieldMDName str = mkName (str ++ "_md")

-- | Pads constructor
mkConstrName   str  = mkName str
mkConstrIMDName str = mkName (str ++ "_imd")
mkfnMDName str      = mkName (strToLower str ++ "_md")


-- ** Naming Parsers

mkTyParserName  str = mkName (strToLower str ++ "_parseM")
mkTyParserSName str = mkName (strToLower str ++ "_parseS")

mkTyParserQName  str = mkName (appendLower str "_parseM")
mkTyParserSQName str = mkName (appendLower str "_parseS")

mkVarParserName str = mkName (strToLower str ++ "__p")


-- ** Naming Printers

getBranchMDNameU str = mkName ((strToUpper str)++"_md")
getBranchMDNameL str = mkName ((strToLower str)++"_md")
getBranchNameU str = mkName (strToUpper str)
getBranchNameL   str = mkName  (strToLower str)
getStructInnerMDName name = let str = show name in mkName (str++"_imd")

mkTyPrinterName str    = mkName (strToLower str ++ "_printFL")
mkTyPrinterQName str    = mkName (appendLower str "_printFL")
mkTyPrinterVarName str = mkName (str ++ "__pr")

mkTyDefName str    = mkName (strToLower str ++ "_def")
mkTyDefQName str    = mkName (appendLower str "_def")
mkTyDefVarName str = mkName (str ++ "__d")


-- ** Naming Generators

mkTyGeneratorName str = mkName (strToLower str ++ "_genM")
mkTyGeneratorQName str = mkName (appendLower str "_genM")
mkVarGeneratorName str = mkName (strToLower str ++ "__g")

-- ** Naming Serializers

mkTySerializerName str = mkName (strToLower str ++ "_serialize")
mkTySerializerQName str = mkName (appendLower str "_serialize")
mkTySerializerVarName str = mkName (str ++ "__s")


appendTo :: QString -> String -> String
appendTo ms s    = qName (init ms ++ [last ms ++ s])
appendLower ms s = qName (init ms ++ [strToLower (last ms) ++ s])

type UString = String
type LString = String

foldl1M :: Monad m => (a -> a -> m a) -> [a] -> m a
foldl1M f (x:xs) = foldM f x xs

foldr1M :: Monad m => (a -> a -> m a) -> [a] -> m a
foldr1M f [x] = return x
foldr1M f (x:xs) = f x =<< foldr1M f xs

appT2 f x y = AppT (AppT f x) y

appE3 f x y z = AppE (AppE (AppE f x) y) z
