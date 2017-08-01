{-# LANGUAGE TupleSections, ViewPatterns, TemplateHaskell, NamedFieldPuns, ScopedTypeVariables,
             RecordWildCards, UndecidableInstances #-}

{-
** *********************************************************************
*                                                                      *
*         (c)  Kathleen Fisher <kathleen.fisher@gmail.com>             *
*              John Launchbury <john.launchbury@gmail.com>             *
*                                                                      *
************************************************************************
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

import Language.Haskell.TH 
import Language.Haskell.Syntax

import Data.Data
import Data.Char
import qualified Data.Map as M
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Control.Monad
import Language.Haskell.TH.Syntax (lift)

import Debug.Trace

type BString = S.RawStream

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
    mpa pd@(PadsDeclObtain n _ _ _)   = [| ($(litE $ stringL n), $(lift pd)) |]
  in listE . (map mpa)

-- | Top level code gen function from Pads decls to Haskell decls with the
-- specified list of type classes for all of the generated Pads types to derive.
make_pads_declarations' :: Derivation -> [PadsDecl] -> Q [Dec]
make_pads_declarations' derivation ds = fmap concat (mapM (genPadsDecl derivation) ds)


----------------------------------------------------------------------------
-- GENERATE DECLARATIONS AND CODE FROM INDIVIDUAL PADS DECLARATIONS
----------------------------------------------------------------------------

genPadsDecl :: Derivation -> PadsDecl -> Q [Dec]
-- ^ Generate all the top level Haskell declarations associated with a single
-- Pads declaration.
genPadsDecl derivation pd@(PadsDeclType name args pat padsTy) = do
  let typeDecs = mkTyRepMDDecl name args padsTy
  parseM  <- genPadsParseM name args pat padsTy
  parseS  <- genPadsParseS name args pat
  printFL <- genPadsPrintFL name args pat padsTy
  def <- genPadsDef name args pat padsTy
  let sigs = mkPadsSignature name args (fmap patType pat)
  ast <- astDecl name pd
  return $ typeDecs ++ parseM ++ parseS ++ printFL ++ def ++ sigs

genPadsDecl derivation pd@(PadsDeclData name args pat padsData derives) = do
  dataDecs <- mkDataRepMDDecl derivation name args padsData derives
  parseM <- genPadsDataParseM name args pat padsData 
  parseS <- genPadsParseS name args pat
  printFL <- genPadsDataPrintFL name args pat padsData
  def <- genPadsDataDef name args pat padsData
  let instances = mkPadsInstance name args (fmap patType pat)
  let sigs = mkPadsSignature name args (fmap patType pat)
  ast <- astDecl name pd
  return $ dataDecs ++ parseM ++ parseS ++ printFL ++ def ++ instances ++ sigs

genPadsDecl derivation pd@(PadsDeclNew name args pat branch derives) = do
  dataDecs <- mkNewRepMDDecl derivation name args branch derives
  parseM <- genPadsNewParseM name args pat branch 
  parseS <- genPadsParseS name args pat
  printFL <- genPadsNewPrintFL name args pat branch
  def <- genPadsNewDef name args pat branch
  let instances = mkPadsInstance name args (fmap patType pat)
  let sigs = mkPadsSignature name args (fmap patType pat)
  ast <- astDecl name pd
  return $ dataDecs ++ parseM ++ parseS ++ printFL ++ def ++ instances ++ sigs

genPadsDecl derivation pd@(PadsDeclObtain name args padsTy exp) = do
  let mdDec = mkObtainMDDecl name args padsTy
  parseM  <- genPadsObtainParseM name args padsTy exp
  parseS  <- genPadsParseS name args Nothing
  printFL <- genPadsObtainPrintFL name args padsTy exp
  def <- genPadsObtainDef name args padsTy exp
  let sigs = mkPadsSignature name args Nothing
  ast <- astDecl name pd
  return $ mdDec ++ parseM ++ parseS ++ printFL ++ def ++ sigs

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


-----------------------------------------------------------
-- GENERATE REP/MD TYPE DECLARATIONS
-----------------------------------------------------------

-- | Make the type declarations for the representation and the metadata of a
-- Pads-defined type, @'PadsTy'@.
mkTyRepMDDecl :: UString -> [UString] -> PadsTy -> [Dec]
mkTyRepMDDecl name args ty = [repType, mdType]
  where
  repType = TySynD (mkRepName name) tyArgs (mkRepTy ty)
  mdType  = TySynD (mkMDName name) tyArgsMD (mkMDTy False ty)
  tyArgs  = map (PlainTV . mkName) args
  tyArgsMD  = map (PlainTV . mkName . (++"_md")) args


-----------------------------------------------------------
-- GENERATE REP/MD DATA DECLARATIONS
-----------------------------------------------------------

-- | Make the data type declarations for the representation and the metadata of
-- a Pads-defined data type, @'PadsData'@.
mkDataRepMDDecl :: Derivation -> UString -> [LString] -> PadsData -> [QString] -> Q [Dec]
mkDataRepMDDecl derivation name args branches ds = do
  bs' <- mapM (return . mkMDUnion) bs
  imdDecl  <- dataD (cxt []) (mkIMDName name) tyArgsMD Nothing bs'  (derive [])
  bs'' <- mapM (return . mkRepUnion) bs
  dataDecl <- dataD (cxt []) (mkRepName name) tyArgs   Nothing bs'' (derive ds)
  derivesData <- derivation dataDecl
  derivesImd <- derivation imdDecl
  return $ [dataDecl, mdDecl, imdDecl] ++ derivesData ++ derivesImd
  where
    mdDecl   = TySynD   (mkMDName name)  tyArgsMD (mkTupleT [ConT ''Base_md, imdApp])
    tyArgs   = map (PlainTV . mkName) args
    tyArgsMD   = map (PlainTV . mkName . (++"_md")) args
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
                | (Just l,(strict,ty),_) <- fields, hasRep ty]

mkMDUnion :: BranchInfo -> Q Con
mkMDUnion (BConstr c args expM) = normalC (mkConstrIMDName c) mds
  where   
    mds = [bangType (mkStrict NotStrict) (return $ mkMDTy False ty) | (_,ty) <- args] --MD , hasRep ty]
mkMDUnion (BRecord c fields expM) = do
  { let lmds = [ do { fn <- genLabMDName "m" lM
                    ; varBangType fn (bangType (mkStrict NotStrict) (return $ mkMDTy False ty))
                    }
               | (lM,(_,ty),_) <- fields
               ]
  ; recC (mkConstrIMDName c) lmds
  }
--MD    lmds <- return [(mkFieldMDName l,NotStrict,mkMDTy ty) | (Just l,(_,ty),_) <- fields, hasRep ty]

derive :: [QString] -> CxtQ
derive ds = cxt (map (conT . mkName . qName) ds
  ++ [conT $ mkName d | d<-["Show","Eq","Typeable","Data","Ord"], not (d `elem` map last ds)])


-----------------------------------------------------------
-- GENERATE REP/MD NEWTYPE DECLARATIONS
-----------------------------------------------------------

mkNewRepMDDecl :: Derivation -> UString -> [LString] -> BranchInfo -> [QString] -> Q [Dec]
mkNewRepMDDecl derivation name args branch ds = do
  imdDecl  <- newtypeD (cxt []) (mkIMDName name) tyArgsMD Nothing (mkMDUnion  branch) (derive [])
  dataDecl <- newtypeD (cxt []) (mkRepName name) tyArgs   Nothing (mkRepUnion branch) (derive ds)
  derivesData <- derivation dataDecl
  derivesImd <- derivation imdDecl
  return $ [dataDecl, mdDecl, imdDecl] ++ derivesData ++ derivesImd
  where
    mdDecl   = TySynD   (mkMDName name)  tyArgsMD (mkTupleT [ConT ''Base_md, imdApp])
    tyArgs   = map (PlainTV . mkName) args
    tyArgsMD   = map (PlainTV . mkName . (++"_md")) args
    imdApp   = foldl AppT (ConT (mkIMDName name)) (map (VarT . mkName . (++"_md")) args)


-----------------------------------------------------------
-- GENERATE MD TYPE FROM OBTAIN DECLARATIONS -- Design decision not to do this
-----------------------------------------------------------

mkObtainMDDecl :: UString -> [UString] -> PadsTy -> [Dec]
mkObtainMDDecl name args ty
  = [mdType]
  where
    mdType  = TySynD (mkMDName name) tyArgsMD (mkMDTy False ty)
    tyArgsMD  = map (PlainTV . mkName . (++"_md")) args


-----------------------------------------------------------
-- GENERATE REPRESENTATION TYPE OF A TYPE EXPRESSION
-----------------------------------------------------------

mkRepTy ::  PadsTy -> Type
mkRepTy ty = case ty of
  PPartition pty exp          -> mkRepTy pty
  PConstrain pat pty exp      -> mkRepTy pty 
  PTransform tySrc tyDest exp -> mkRepTy tyDest 
  PList ty sep term           -> ListT `AppT` mkRepTy ty
  PValue exp pty              -> mkRepTy pty 
  PApp tys expM               -> foldl1 AppT [mkRepTy ty | ty <- tys, hasRep ty]
  PTuple tys                  -> mkRepTuple tys
  PExpression _               -> ConT ''()
  PTycon c                    -> ConT (mkRepQName c)
  PTyvar v                    -> VarT (mkName v)  

mkRepTuple :: [PadsTy] -> Type
mkRepTuple tys = case reps of  
    []     -> ConT ''()
    [ty]   -> ty
    (t:ts) -> mkTupleT reps
  where
    reps = [mkRepTy ty | ty <- tys, hasRep ty]


-----------------------------------------------------------
-- GENERATE META-DATA REPRESENTATION OF TYPE EXPRESSION
-----------------------------------------------------------

mkMDTy :: Bool -> PadsTy -> Type
mkMDTy isMeta ty = case ty of
  PPartition pty exp      -> mkMDTy isMeta pty
  PConstrain pat pty exp  -> mkMDTy isMeta pty 
  PTransform src dest exp -> mkMDTy isMeta dest 
  PList ty sep term       -> mkTupleT [ConT ''Base_md, ListT `AppT` mkMDTy isMeta ty]
  PValue exp pty          -> mkMDTy isMeta pty 
  PApp tys expM           -> foldl1 AppT [mkMDTy isMeta ty | ty <- tys] --MD , hasRep ty]
  PTuple tys              -> mkMDTuple isMeta tys
  PExpression _           -> ConT ''Base_md
  PTycon c                -> ConT (mkMDQName c)
  PTyvar v                -> if isMeta
    then AppT (ConT ''Meta) (VarT $ mkName v)
    else VarT (mkName $ v ++ "_md")  

mkMDTuple :: Bool -> [PadsTy] -> Type
mkMDTuple isMeta tys = case mds of  
    []     -> ConT ''Base_md
    [m]    -> mkTupleT [ConT ''Base_md, m] 
    (m:ms) -> mkTupleT [ConT ''Base_md, mkTupleT mds]
  where
    mds = [mkMDTy isMeta ty | ty <- tys] --MD , hasRep ty]


-----------------------------------------------------------------
-- GENERATING INSTANCE DECLARATIONS FROM DATA/NEW DECLARATION
------------------------------------------------------------------

mkPadsInstance :: UString -> [LString] -> Maybe Type -> [Dec]
mkPadsInstance str args mb@(Nothing)
  = buildInst mb str args (ConT ''Pads)
mkPadsInstance str args mb@(Just ety) 
  = buildInst mb str args (ConT ''Pads1 `AppT` ety)

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
  printFL_method = FunD 'printFL1 [Clause mbarg (NormalB (applyE (VarE (mkTyPrinterName str) : [VarE 'printFL | a <- args]))) []]
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


mkPadsSignature :: UString -> [LString] -> Maybe Type -> [Dec]
mkPadsSignature str args mb@(Nothing)
  = buildSignature mb str args (ConT ''Pads)
mkPadsSignature str args mb@(Just ety) 
  = buildSignature mb str args (ConT ''Pads1 `AppT` ety)

buildSignature mb str args pads = [printFL_signature,def_signature]
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
-----------------------------------------------------------------
-- GENERATING PARSER DECLARATION FROM TYPE/DATA/NEW DECLARATION
------------------------------------------------------------------

genPadsParseM :: UString -> [LString] -> Maybe Pat -> PadsTy -> Q [Dec]
genPadsParseM name args patM padsTy = do 
  body  <- genParseTy padsTy
  return [mkParserFunction name args patM body]

genPadsDataParseM :: UString -> [LString] -> (Maybe Pat) -> PadsData -> Q [Dec] 
genPadsDataParseM name args patM padsData = do 
  body  <- genParseData padsData
  return [mkParserFunction name args patM body]

genPadsNewParseM :: UString -> [LString] -> (Maybe Pat) -> BranchInfo -> Q [Dec] 
genPadsNewParseM name args patM branch = do 
  (dec,exp) <- genParseBranchInfo branch
  let body = LetE [dec] exp
  return [mkParserFunction name args patM body]

genPadsObtainParseM :: UString -> [LString] -> PadsTy -> Exp -> Q [Dec]
genPadsObtainParseM name args padsTy exp = do
  body  <- genParseTy (PTransform padsTy (PTycon [name]) exp)
  return [mkParserFunction name args Nothing body]

mkParserFunction :: UString -> [LString] -> Maybe Pat -> Exp -> Dec
mkParserFunction name args patM body
  = FunD parserName [Clause parserArgs (NormalB body) []]
  where
    parserName = mkTyParserName name    
    parserArgs = map (VarP . mkVarParserName) args ++ Maybe.maybeToList patM


--------------------------------------------------------------
-- GENERATING STRING-PARSER DECLARATION
--------------------------------------------------------------

genPadsParseS :: UString -> [LString] -> Maybe Pat -> Q [Dec]
genPadsParseS name args patM = do 
  { body <- [| parseStringInput $(return parserWithArgs) |]
  ; return [ FunD (mkTyParserSName name) [Clause parserArgs (NormalB body) []] ]
  }
  where
    parserWithArgs = foldl1 AppE (VarE parserName : map patToExp parserArgs)
    parserName     = mkTyParserName name    
    parserArgs     = map (VarP . mkVarParserName) args ++ Maybe.maybeToList patM


------------------------------------------------------
-- GENERATING PARSER FROM TYPE EXPRESSION
------------------------------------------------------

genParseTy :: PadsTy -> Q Exp
genParseTy pty = case pty of
    PConstrain pat ty exp   -> genParseConstrain (return pat) ty (return exp)
    PTransform src dest exp -> genParseTyTrans src dest (return exp)
    PList ty sep term       -> genParseList ty sep term
    PPartition ty exp       -> genParsePartition ty exp
    PValue exp ty           -> genParseValue exp
    PApp tys argE           -> genParseTyApp tys argE
    PTuple tys              -> genParseTuple tys
    PExpression exp         -> genParseExp exp
    PTycon c                -> return $ mkParseTycon c
    PTyvar v                -> return $ mkParseTyvar v

genParseConstrain :: Q Pat -> PadsTy -> Q Exp -> Q Exp
genParseConstrain patQ ty expQ = [| parseConstraint $(genParseTy ty) $pred |]
  where
    pred = lamE [patQ, varP (mkName "md")] expQ

genParseTyTrans :: PadsTy -> PadsTy -> Q Exp -> Q Exp
genParseTyTrans src dest expQ
  = [| parseTransform $(genParseTy src) (fst $expQ) |]

genParseList :: PadsTy -> (Maybe PadsTy) -> (Maybe TermCond) -> Q Exp
genParseList ty sep term =
  case (sep,term) of 
    (Nothing,  Nothing)          -> [| parseListNoSepNoTerm $(genParseTy ty) |]
    (Just sep, Nothing)          -> [| parseListSepNoTerm $(genParseTy sep) $(genParseTy ty) |]
    (Nothing,  Just (LLen lenE)) -> [| parseListNoSepLength $(return lenE) $(genParseTy ty) |]
    (Just sep, Just (LLen lenE)) -> [| parseListSepLength $(genParseTy sep) $(return lenE) $(genParseTy ty) |]
    (Nothing,  Just (LTerm term))-> [| parseListNoSepTerm $(genParseTy term) $(genParseTy ty) |]
    (Just sep, Just (LTerm term))-> [| parseListSepTerm $(genParseTy sep) $(genParseTy term) $(genParseTy ty) |]

genParsePartition :: PadsTy -> Exp -> Q Exp
genParsePartition ty disc = [| parsePartition $(genParseTy ty) $(return disc) |]

genParseValue :: Exp -> Q Exp
genParseValue exp = return $ AppE (VarE 'return) (TupE [exp,VarE 'cleanBasePD])

genParseTuple :: [PadsTy] -> Q Exp
genParseTuple []  = [| return ((), cleanBasePD) |]
genParseTuple tys = do
  { let f_rep = buildF_rep vars_frep
  ; let f_md  = buildF_md vars_fmd vars_frep 
  ; body  <- foldl parseNext [| return ($(dyn "f_rep"),$(dyn "f_md")) |] tys
  ; return (LetE [f_rep,f_md] body)
  }
  where
    vars_frep = [v | (v,t) <- zip vars_fmd tys, hasRep t]
    vars_fmd  = [ mkName ("x"++show n) | n <- [1 .. length tys]] 

parseNext :: Q Exp -> PadsTy -> Q Exp
parseNext prog t
  | hasRep t  = [| $prog =@= $(genParseTy t) |]
  | otherwise = [| $prog =@  $(genParseTy t) |]

buildF_rep :: [Name] -> Dec
buildF_rep vars_frep
  = FunD (mkName "f_rep") [Clause
         (map VarP vars_frep) (NormalB (TupE (map VarE vars_frep))) [] ]

buildF_md :: [Name] -> [Name] -> Dec
buildF_md vars_fmd vars_frep 
  = FunD (mkName "f_md") [Clause (map VarP vars_fmd) (NormalB body) []]
  where
    mdHeaders = [ VarE 'get_md_header `AppE` VarE xi | xi <- vars_fmd ]
    body = TupE [mkMergeBaseMDs mdHeaders, TupE (map VarE vars_fmd)] --vars_frep)]

mkMergeBaseMDs :: [Exp] -> Exp
mkMergeBaseMDs [e] = e
mkMergeBaseMDs es  = VarE 'mergeBaseMDs `AppE` ListE es


genParseExp :: Exp -> Q Exp
genParseExp exp                = [| litParse $(return exp) |]

genParseTyApp :: [PadsTy] -> Maybe Exp -> Q Exp
genParseTyApp tys expM = do
  fs <- mapM genParseTy tys
  return (foldl1 AppE (fs ++ Maybe.maybeToList expM))

mkParseTycon :: QString -> Exp
mkParseTycon ["EOF"] = VarE 'eof_parseM
mkParseTycon ["EOR"] = VarE 'eor_parseM
mkParseTycon c     = VarE (mkTyParserQName c)

mkParseTyvar :: String -> Exp
mkParseTyvar v = VarE (mkVarParserName v) -- should gensym these, but probably ok


----------------------------------------------------------
-- GENERATING PARSERS FROM UNION/SWITCH EXPRESSIONS
----------------------------------------------------------

genParseData :: PadsData -> Q Exp
genParseData (PUnion bs)       = genParseUnion bs
genParseData (PSwitch exp pbs) = genParseSwitch exp pbs

genParseUnion :: [BranchInfo] -> Q Exp
genParseUnion bs = do
  { (decs,bodies) <- fmap unzip $ mapM genParseBranchInfo bs
  ; let body = case bodies of
                 [b] -> b
                 bs  -> (VarE 'choiceP) `AppE` (ListE bs)
  ; return (LetE decs body)
  }

genParseSwitch :: Exp -> [(Pat,BranchInfo)] -> Q Exp
genParseSwitch exp pbs = do
  { let (ps,bs) = unzip pbs
  ; (decs,bodies) <- fmap unzip $ mapM genParseBranchInfo bs
  ; let body = CaseE exp [Match p (NormalB b) [] | (p,b) <- zip ps bodies]
  ; return (LetE decs body)
  }

genParseBranchInfo :: BranchInfo -> Q (Dec,Exp)
genParseBranchInfo (BRecord c fields pred) = genParseRecord c fields pred
genParseBranchInfo (BConstr c args pred) = do
  { body <- foldl parseNext [| return ($(conE (mkConstrName c)),$(varE (mkfnMDName c))) |] tys
  ; return (con_md, body)
  }
  where
    tys  = [ty | (strict,ty) <- args]
    con_md = buildConstr_md (mkfnMDName c) (ConE (mkConstrIMDName c)) tys

buildConstr_md :: Name -> Exp -> [PadsTy] -> Dec
buildConstr_md fnMD conMD tys 
  = FunD fnMD [Clause (map VarP vars_fmd) (NormalB body) []]
  where
    vars_fmd   = [ mkName ("x"++show n) | n <- [1 .. length tys]] 
    mdHeaders  = [ VarE 'get_md_header `AppE` VarE xi | xi <- vars_fmd ]
    body       = TupE [mkMergeBaseMDs mdHeaders, applyE (conMD : map VarE vars_conmd)]
    vars_conmd = vars_fmd --MD [v | (v,t) <- zip vars_fmd tys, hasRep t]


----------------------------------------------------------
-- GENERATING PARSERS FROM RECORD EXPRESSIONS
----------------------------------------------------------

genParseRecord :: UString -> [FieldInfo] -> (Maybe Exp) -> Q (Dec,Exp)
genParseRecord c fields pred = do
  { c_md <- newName (strToLower c)
  ; let con_md = buildConstr_md c_md (ConE (mkConstrIMDName c))
                       [ty | (_,(_,ty),_) <- fields]
  ; labMDs  <- sequence [genLabMDName "x" l | (l,(_,_),_) <- fields] 
  ; let fnMDLabs  = applyE $ map VarE (c_md : labMDs)
  ; doStmts <- sequence [genParseField f xn | (f,xn) <- zip fields labMDs]
  ; let labs = [mkName lab | (Just lab,(_,ty),_) <- fields, hasRep ty]
  ; let conLabs = applyE (ConE (mkConstrName c) : map VarE labs)
  ; returnStmt <- [| return ($(return conLabs),$(return fnMDLabs)) |]
  ; return (con_md, DoE (doStmts ++ [NoBindS returnStmt]))
  }
  where

genLabMDName :: String -> Maybe String -> Q Name
genLabMDName s (Just lab) = return (mkFieldMDName lab)
genLabMDName s Nothing    = liftM mangleName (newName s)

genParseField :: FieldInfo -> Name -> Q Stmt
genParseField (labM, (strict, ty), expM) xn = do
  { parseTy <- case expM of 
                Nothing  -> genParseTy ty
                Just exp -> genParseRecConstrain labP (VarP xn) ty exp
  ; return (BindS (TupP [labP, VarP xn]) parseTy)
  }
  where
    labP = case labM of
              Just lab -> VarP (mkName lab)
              Nothing  -> WildP

genParseRecConstrain :: Pat -> Pat -> PadsTy -> Exp -> Q Exp
genParseRecConstrain labP xnP ty exp = [| parseConstraint $(genParseTy ty) $pred |]
  where
    pred = return (LamE [labP, xnP] exp)



----------------------------------------------------
-- GENERATING PRINTING FUNCTION FROM A DECLARATION
----------------------------------------------------

genPadsPrintFL :: UString -> [LString] -> Maybe Pat -> PadsTy -> Q [Dec]
genPadsPrintFL name args patM padsTy = do 
  let rm = [mkName "rep", mkName "md"]
  body  <- genPrintTy padsTy $ Just $ TupE (map VarE rm)
  return [mkPrinterFunction name args rm patM body]

genPadsDataPrintFL :: UString -> [LString] -> Maybe Pat -> PadsData -> Q [Dec] 
genPadsDataPrintFL name args patM padsData = do
  let rm = [mkName "rep", mkName "md"]
  body  <- genPrintData padsData $ Just $ TupE (map VarE rm)
  return [mkPrinterFunction name args rm patM body]

genPadsNewPrintFL :: UString -> [LString] -> Maybe Pat -> BranchInfo -> Q [Dec] 
genPadsNewPrintFL name args patM branch = do 
  let rm = [mkName "rep", mkName "md"]
  matches <- genPrintBranchInfo False branch
  let body = CaseE (TupE (map VarE rm)) matches
  return [mkPrinterFunction name args rm patM body]

genPadsObtainPrintFL :: UString -> [LString] -> PadsTy -> Exp -> Q [Dec]
genPadsObtainPrintFL name args padsTy exp = do
  let rm = [mkName "rep", mkName "md"]
  body  <- genPrintTy (PTransform padsTy (PTycon [name]) exp) $ Just $ TupE (map VarE rm)
  return [mkPrinterFunction name args rm Nothing body]

mkPrinterFunction :: UString -> [LString] -> [Name] -> Maybe Pat -> Exp -> Dec
mkPrinterFunction name args rm patM body =
  FunD printerName [Clause (printerArgs ++ [TupP (map VarP rm)]) (NormalB body) []]
  where
  printerName = mkTyPrinterName name
  printerArgs = map (VarP . mkTyPrinterVarName) args ++ Maybe.maybeToList patM

------------------------------------------
-- GENERATE PRINTING FUNCTION FROM A TYPE
------------------------------------------

genPrintTy :: PadsTy -> Maybe Exp -> Q Exp
genPrintTy (PConstrain pat ty exp) rm   = genPrintTy ty rm  -- XXX: doesn't check the constraint; ideally we should change @printFL@ to account for possible printing errors
genPrintTy (PTransform src dest exp) rm = genPrintTrans src exp rm
genPrintTy (PList ty sepM termM) rm     = genPrintList ty sepM termM >>= applyPrintTy rm
genPrintTy (PPartition ty exp) rm       = [| (error "genPrintTy PPartition not implemented") |] --genPrintPartition ty exp rm
genPrintTy (PApp tys expM) rm           = genPrintTyApp tys expM >>= applyPrintTy rm
genPrintTy (PTuple tys) rm              = genPrintTuple tys rm
genPrintTy (PExpression exp) rm         = genPrintExp exp rm
genPrintTy (PTycon c) rm                = genPrintTycon c >>= applyPrintTy rm
genPrintTy (PTyvar v) rm                = genPrintTyVar v >>= applyPrintTy rm
genPrintTy (PValue exp ty) rm           = genPrintValue exp rm

genPrintValue :: Exp -> Maybe Exp -> Q Exp
genPrintValue exp rm = return $ VarE 'nil

genPrintTrans :: PadsTy -> Exp -> Maybe Exp -> Q Exp
genPrintTrans tySrc exp Nothing
  = genPrintTy tySrc Nothing
genPrintTrans tySrc exp (Just rm) = do
  rm' <- [| snd $(return exp) $(return rm) |]
  genPrintTy tySrc (Just rm')

applyPrintTy :: Maybe Exp -> Exp -> Q Exp
applyPrintTy rm f = do
  case rm of
    Nothing -> return f
    Just repmdE -> return $ AppE f repmdE

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

genPrintTyApp :: [PadsTy] -> Maybe Exp -> Q Exp
genPrintTyApp tys expM = do
  prtys <- mapM (flip genPrintTy Nothing) tys
  foldl1M (\e1 e2 -> return $ AppE e1 e2) (prtys ++ Maybe.maybeToList expM)

{-
intPair_printFL (r,m)
  = case (r,m) of
      ((r1,r2),(_,(m1,m2)))
        -> int_PrintFL (r1,m1) +++
           addString "|" +++
           int_PrintFL (r2,m2)
-}

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

-- filters patterns for representations that do not appear in-memory
filterByHasRep :: [PadsTy] -> [a] -> [a]
filterByHasRep tys xs = map snd $ filter (hasRep . fst) (zip tys xs)

genNamesforTuple :: Bool -> String -> [PadsTy] -> Q [Maybe Name]
genNamesforTuple False str tys = sequence [fmap Just (newName str) | ty <- tys]
genNamesforTuple True str tys = sequence [if hasRep ty then fmap Just (newName str) else return Nothing | ty <- tys]

genPrintTupleInner t (Just r) (Just m) = genPrintTy t (Just (TupE [VarE r,VarE m])) 
genPrintTupleInner t Nothing (Just m) = do
  def <- genDefTy t
  genPrintTy t (Just (TupE [def,VarE m])) 
genPrintTupleInner t Nothing Nothing   = genPrintTy t Nothing

genPrintExp :: Exp -> Maybe Exp -> Q Exp
genPrintExp e _ = [| litPrint $(return e) |]

genPrintTycon :: QString -> Q Exp
genPrintTycon c = return $ VarE (mkTyPrinterQName c)

genPrintTyVar :: LString -> Q Exp
genPrintTyVar v = return $ VarE (mkTyPrinterVarName v)

------------------------------------------
-- GENERATE PRINTING FUNCTION FROM A DATAYPE
------------------------------------------

genPrintData :: PadsData -> Maybe Exp -> Q Exp
genPrintData (PUnion bs) rm = genPrintUnion bs rm
genPrintData (PSwitch exp pbs) rm = genPrintSwitch exp pbs rm

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

genPrintBranchInfo :: Bool -> BranchInfo -> Q [Match]
genPrintBranchInfo doDef (BRecord c fields predM) =  genPrintRecord c fields predM
genPrintBranchInfo doDef (BConstr c args predM) = genPrintConstr doDef c args predM

genPrintRecord :: UString -> [FieldInfo] -> Maybe Exp -> Q [Match]
genPrintRecord (mkName -> recName) fields predM = do 
  (repEs, repPs) <- getPEforFields (\t -> genDefTy t >>= \def -> return $ SigE def (mkRepTy t)) (return . getBranchNameL) fields
  (mdEs,  mdPs)  <- getPEforFields (return . SigE (VarE 'myempty) . mkMDTy False) (return . getBranchMDNameL) fields
  let ptys = map (\(n,(_,ty),p) -> ty) fields
  let ty_rep_mds = zip3 ptys repEs mdEs
  expE <- mapM (\(ty,r,m) -> genPrintTy ty $ Just $ TupE [r,m]) ty_rep_mds
  let printItemsE = ListE expE
  let caseBody = NormalB (AppE (VarE 'concatFL) printItemsE)
  let mdPat  = TupP[WildP, RecP (getStructInnerMDName recName) mdPs]
  let repPat = RecP recName repPs
  let casePat = TupP [repPat, mdPat]
  let match = Match casePat caseBody []
  return [match]

getPEforField :: (PadsTy -> Q Exp) -> (String -> Q Name) -> FieldInfo -> Q (Exp, Maybe FieldPat)
getPEforField def mkFieldNm (nameOpt, (strict,pty), optPred) = case nameOpt of
  Nothing -> def pty >>= \d -> return (d,Nothing)
  Just str -> do
    name <- mkFieldNm str
    let (varE, varP) = genPE name
    return (varE, Just (name, varP))

getPEforFields :: (PadsTy -> Q Exp) -> (String -> Q Name) -> [FieldInfo] -> Q ([Exp], [FieldPat])
getPEforFields def mkFieldNm fields = do
  eps <- mapM (getPEforField def mkFieldNm) fields
  let (es, pOpts) = List.unzip eps
      ps = Maybe.catMaybes pOpts
  return (es, ps)

genPrintConstr :: Bool -> String -> [ConstrArg] -> (Maybe Exp) -> Q [Match]
genPrintConstr doDef (mkName -> recName) args predM = do
  let fields = map (\c -> (Just "arg",c,Nothing)) args
  (repEs, repPs) <- getPEforFields (\t -> genDefTy t >>= \def -> return $ SigE def (mkRepTy t)) newName fields
  (mdEs,  mdPs)  <- getPEforFields (return . SigE (VarE 'myempty) . mkMDTy False) newName fields
  let ptys = map (\(n,(s,ty),p) -> ty) fields

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

genPrintSwitch :: Exp -> [(Pat,BranchInfo)] -> Maybe Exp -> Q Exp
genPrintSwitch exp pbs rm = genPrintUnion (map snd pbs) rm

----------------------------------------------------
-- GENERATING DEFAULT FUNCTION FROM A DECLARATION
----------------------------------------------------

genPadsDef :: UString -> [LString] -> Maybe Pat -> PadsTy -> Q [Dec]
genPadsDef name args patM padsTy = do 
  body  <- genDefTy padsTy
  return [mkDefFunction name args patM body]

genPadsDataDef :: UString -> [LString] -> Maybe Pat -> PadsData -> Q [Dec] 
genPadsDataDef name args patM padsData = do
  body  <- genDefData padsData
  return [mkDefFunction name args patM body]

genPadsNewDef :: UString -> [LString] -> Maybe Pat -> BranchInfo -> Q [Dec] 
genPadsNewDef name args patM branch = do 
  body <- genDefBranchInfo branch
  return [mkDefFunction name args patM body]

genPadsObtainDef :: UString -> [LString] -> PadsTy -> Exp -> Q [Dec]
genPadsObtainDef name args padsTy exp = do
  body  <- genDefTy (PTransform padsTy (PTycon [name]) exp)
  return [mkDefFunction name args Nothing body]

mkDefFunction :: UString -> [LString] -> Maybe Pat -> Exp -> Dec
mkDefFunction name args patM body =
  FunD defName [Clause (defArgs) (NormalB body) []]
  where
  defName = mkTyDefName name
  defArgs = map (VarP . mkTyDefVarName) args ++ Maybe.maybeToList patM

------------------------------------------
-- GENERATE DEFAULT FUNCTION FROM A TYPE
------------------------------------------

genDefTy :: PadsTy -> Q Exp
genDefTy (PConstrain pat ty exp)   = genDefTy ty  -- XXX: doesn't check the constraint; ideally we should change @printFL@ to account for possible printing errors
genDefTy (PTransform src dest exp) = do
  defSrc <- genDefTy src
  srcToDest <- [| \rep -> fst $ (fst $(return exp)) S.zeroPos (rep,(error "TODO defaultMd")) |] -- XXX: fix this undefined, it kind of requires defaultMd to be defined inductively over Pads types as well...
  return $ AppE srcToDest defSrc
genDefTy (PList ty sepM termM)     = [| [] |]
genDefTy (PPartition ty exp)       = genDefTy ty
genDefTy (PApp tys expM)           = genDefTyApp tys expM
genDefTy (PTuple tys)              = genDefTuple tys
genDefTy (PExpression exp)         = genDefExp exp
genDefTy (PTycon c)                = genDefTycon c
genDefTy (PTyvar v)                = genDefTyVar v
genDefTy (PValue exp ty)           = genDefTy ty

genDefValue :: Exp -> Q Exp
genDefValue exp = return exp

genDefTyApp :: [PadsTy] -> Maybe Exp -> Q Exp
genDefTyApp tys expM = do
  prtys <- mapM genDefTy tys
  foldl1M (\e1 e2 -> return $ AppE e1 e2) (prtys ++ Maybe.maybeToList expM)

genDefTuple :: [PadsTy] -> Q Exp
genDefTuple tys = case reps of
  [] -> [| () |]
  [ty] -> genDefTy ty
  tys -> do
    exps <- mapM genDefTy tys
    return $ TupE exps
  where
  reps = [ty | ty <- tys, hasRep ty]

genDefExp :: Exp -> Q Exp
genDefExp e = return e

genDefTycon :: QString -> Q Exp
genDefTycon c = return $ VarE (mkTyDefQName c)

genDefTyVar :: LString -> Q Exp
genDefTyVar v = return $ VarE (mkTyDefVarName v)

------------------------------------------
-- GENERATE DEFAULT FUNCTION FROM A DATAYPE
------------------------------------------

genDefData :: PadsData -> Q Exp
genDefData (PUnion bs) = genDefBranchInfo (head bs)
genDefData (PSwitch exp pbs) = genDefBranchInfo (snd $ head pbs)

genDefBranchInfo :: BranchInfo -> Q Exp
genDefBranchInfo (BConstr c args pred) = do
  reps <- sequence $ [genDefTy ty | (strict,ty) <- args, hasRep ty]
  return $ foldl1 AppE (ConE (mkConstrName c):reps)
genDefBranchInfo (BRecord c fields expM) = do
  reps <- sequence $ [liftM (l,) (genDefTy ty) | (Just l,(strict,ty),_) <- fields, hasRep ty]

  let lets = flip map reps $ \(lab,def) -> ValD (VarP $ mkName lab) (NormalB def) []
  return $ LetE lets $ foldl1 AppE (ConE (mkConstrName c):map (VarE . mkName . fst) reps)

------------------------------------
-- Name manipulation functions 
------------------------------------

-- Naming types, and accessing the names of types

mkRepName :: String -> Name
mkRepName str = mkName str

mkRepQName :: QString -> Name
mkRepQName str = mkName (qName str)

mkMDName :: String -> Name
mkMDName str = mkName (str ++ "_md")

mkMDQName :: QString -> Name
mkMDQName str = mkName (appendTo str "_md")

mkIMDName name  = mkName (name ++ "_imd")
mkMDVarName name = mkName (name ++ "_md")


-- Naming fields and constructors

mkFieldName str   = mkName str
mkFieldMDName str = mkName (str ++ "_md")



mkConstrName   str  = mkName str
mkConstrIMDName str = mkName (str ++ "_imd")
mkfnMDName str      = mkName (strToLower str ++ "_md")


-- Naming Parsers

mkTyParserName  str = mkName (strToLower str ++ "_parseM")
mkTyParserSName str = mkName (strToLower str ++ "_parseS")

mkTyParserQName  str = mkName (appendLower str "_parseM")
mkTyParserSQName str = mkName (appendLower str "_parseS")

mkVarParserName str = mkName (strToLower str ++ "__p")


-- Naming Printers

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
appE4 f x y z w = AppE (AppE (AppE (AppE f x) y) z) w


