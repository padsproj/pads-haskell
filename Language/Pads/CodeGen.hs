{-# LANGUAGE TemplateHaskell, NamedFieldPuns, ScopedTypeVariables, RecordWildCards #-}


module Language.Pads.CodeGen where

import Language.Pads.Syntax as PS
import Language.Pads.MetaData
import Language.Pads.Generic
import Language.Pads.PadsParser
import Language.Pads.CoreBaseTypes
import Language.Pads.TH
import qualified Language.Pads.Errors as E
import qualified Language.Pads.Source as S
import Language.Pads.LazyList

import Language.Haskell.TH 
import Language.Haskell.Syntax

import Data.Data
import Data.Char
import qualified Data.Map as M
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Control.Monad

type BString = S.RawStream




make_pads_declarations :: [PadsDecl] -> Q [Dec]
make_pads_declarations ds = fmap concat (mapM genPadsDecl ds)


----------------------------------------------------------------------------
-- GENERATE DECLARATIONS AND CODE FROM INDIVIDUAL PADS DECLARATIONS
----------------------------------------------------------------------------

genPadsDecl :: PadsDecl -> Q [Dec]

genPadsDecl (PadsDeclType name args pat padsTy) = do
  { let typeDecs = mkTyRepMDDecl name args padsTy
  ; parseM <- genPadsParseM name args pat padsTy
--  ; parseS <- genPadsParseS name args pat
  ; return (typeDecs ++ parseM ) -- ++ parseS) --  ++ printFL)
  }

genPadsDecl (PadsDeclData name args pat padsData derives) = do
  { let dataDecs = mkDataRepMDDecl name args padsData derives
  ; parseM  <- genPadsDataParseM name args pat padsData 
--  ; parseS <- genPadsParseS name args pat
  ; return (dataDecs ++ parseM) -- ++ parseS) --  ++ instances ) --  ++ printFL)
  }

genPadsDecl (PadsDeclNew name args pat branch derives) = do
  { let dataDecs = mkNewRepMDDecl name args branch derives
  ; parseM  <- genPadsNewParseM name args pat branch 
  ; return (dataDecs ++ parseM) --  ++ instances ++ parseS) --  ++ printFL)
  }


-----------------------------------------------------------
-- GENERATE REP/MD TYPE DECLARATIONS
-----------------------------------------------------------

mkTyRepMDDecl :: UString -> [UString] -> PadsTy -> [Dec]
mkTyRepMDDecl name args ty 
  = [repType, mdType]
  where
    repType = TySynD (mkRepName name) tyArgs (mkRepTy ty)
    mdType  = TySynD (mkMDName name) tyArgs (mkMDTy ty)
    tyArgs  = map (PlainTV . mkName) args


-----------------------------------------------------------
-- GENERATE REP/MD DATA DECLARATIONS
-----------------------------------------------------------

mkDataRepMDDecl :: UString -> [LString] -> PadsData -> [UString] -> [Dec]
mkDataRepMDDecl name args branches ds
  = [dataDecl, mdDecl, imdDecl]
  where
    dataDecl = DataD [] (mkRepName name) tyArgs (map mkRepUnion bs) (derive ds)
    imdDecl  = DataD [] (mkIMDName name) tyArgs (map mkMDUnion bs) (derive [])
    mdDecl   = TySynD   (mkMDName name)  tyArgs (mkTupleT [ConT ''Base_md, imdApp])
    tyArgs   = map (PlainTV . mkName) args
    imdApp   = foldl AppT (ConT (mkIMDName name)) (map (VarT . mkName) args)
    bs       = case branches of
                 PUnion bnchs    -> bnchs
                 PSwitch exp pbs -> [b | (p,b) <- pbs]

mkRepUnion :: BranchInfo -> Con
mkRepUnion (BConstr c args expM) = NormalC (mkConstrName c) reps
  where   
    reps = [(strict,mkRepTy ty) | (strict,ty) <- args, hasRep ty]
mkRepUnion (BRecord c fields expM) = RecC (mkConstrName c) lreps
  where   
    lreps = [(mkName l,strict,mkRepTy ty) | (Just l,(strict,ty),expM) <- fields]

mkMDUnion :: BranchInfo -> Con
mkMDUnion (BConstr c args expM) = NormalC (mkConstrMDName c) mds
  where   
    mds = [(NotStrict,mkMDTy ty) | (_,ty) <- args, hasRep ty]
mkMDUnion (BRecord c fields expM) = RecC (mkConstrMDName c) lmds
  where   
    lmds = [(mkFieldMDName l,NotStrict,mkMDTy ty) | (Just l,(_,ty),expM) <- fields]

derive :: [UString] -> [Name]
derive ds =  map mkName ds
  ++ [mkName d | d<-["Show","Eq","Typeable","Data","Ord"], not (d `elem` ds)]


-----------------------------------------------------------
-- GENERATE REP/MD NEWTYPE DECLARATIONS
-----------------------------------------------------------

mkNewRepMDDecl :: UString -> [LString] -> BranchInfo -> [UString] -> [Dec]
mkNewRepMDDecl name args branch ds
  = [dataDecl, mdDecl, imdDecl]
  where
    dataDecl = NewtypeD [] (mkRepName name) tyArgs (mkRepUnion branch) (derive ds)
    imdDecl  = NewtypeD [] (mkIMDName name) tyArgs (mkMDUnion branch) (derive [])
    mdDecl   = TySynD   (mkMDName name)  tyArgs (mkTupleT [ConT ''Base_md, imdApp])
    tyArgs   = map (PlainTV . mkName) args
    imdApp   = foldl AppT (ConT (mkIMDName name)) (map (VarT . mkName) args)


-----------------------------------------------------------
-- GENERATE TYPE REPRESENTATION OF TYPE EXPRESSION
-----------------------------------------------------------

mkRepTy ::  PadsTy -> Type
mkRepTy ty = case ty of
  PConstrain pat pty exp      -> mkRepTy pty 
  PTransform tySrc tyDest exp -> mkRepTy tyDest 
  PList ty sep term           -> mkRepList ty
  PApp tys expM               -> mkRepApp tys
  PTuple tys                  -> mkRepTuple tys
  PExpression _               -> ConT ''()
  PTycon c                    -> ConT (mkRepName c)
  PTyvar v                    -> VarT (mkName v)

mkRepList :: PadsTy -> Type
mkRepList ty = AppT ListT (mkRepTy ty)

mkRepApp :: [PadsTy] -> Type
mkRepApp tys = foldl1 AppT [mkRepTy ty | ty <- tys, hasRep ty]

mkRepTuple :: [PadsTy] -> Type
mkRepTuple tys = case reps of  
    []     -> ConT ''()
    [ty]   -> ty
    (t:ts) -> mkTupleT reps
  where
    reps = [mkRepTy ty | ty <- tys, hasRep ty]
  
hasRep :: PadsTy -> Bool
hasRep (PExpression l) = False
hasRep (PTycon "EOF")  = False
hasRep (PTycon "EOR")  = False
hasRep _               = True


-----------------------------------------------------------
-- GENERATE META-DATA REPRESENTATION OF TYPE EXPRESSION
-----------------------------------------------------------

mkMDTy ::  PadsTy -> Type
mkMDTy ty = case ty of
  PConstrain pat pty exp      -> mkMDTy pty 
  PTransform tySrc tyDest exp -> mkMDTy tyDest 
  PList ty sep term           -> mkMDList ty
  PApp tys expM               -> mkMDApp tys
  PTuple tys                  -> mkMDTuple tys
  PExpression _               -> ConT ''Base_md
  PTycon c                    -> ConT (mkMDName c)
  PTyvar v                    -> VarT (mkName v)

mkMDList :: PadsTy -> Type
mkMDList ty = mkTupleT [ConT ''Base_md, AppT ListT (mkMDTy ty)]    

mkMDApp :: [PadsTy] -> Type
mkMDApp tys = foldl AppT m ms
  where
    (m:ms) = [mkMDTy ty | ty <- tys, hasRep ty]

mkMDTuple :: [PadsTy] -> Type
mkMDTuple tys = case mds of  
    []     -> mkTupleT [ConT ''Base_md, ConT ''Base_md]
    [m]    -> mkTupleT [ConT ''Base_md, m]
    (m:ms) -> mkTupleT [ConT ''Base_md, mkTupleT mds]
  where
    mds = [mkMDTy ty | ty <- tys, hasRep ty]


--------------------------------------------------------------
-- GENERATING PARSER DECLARATION FROM TYPE DECLARATION
--------------------------------------------------------------

genPadsParseM :: UString -> [LString] -> Maybe Pat -> PadsTy -> Q [Dec]
genPadsParseM name args patM padsTy = do 
  { body  <- genParseTy padsTy
  ; return [FunD parser_name [Clause parserArgs (NormalB body) []] ]
  }
  where
    (parser_name,parserArgs) = mkParserNameArgs name args patM

mkParserNameArgs :: UString -> [LString] -> Maybe Pat -> (Name, [Pat])
mkParserNameArgs name args patM = (parserName, parserArgs)
  where
    parserName = mkTyParserName name    
    parserArgs = map (VarP . mkVarParserName) args ++ pat
    pat = Maybe.maybeToList patM

{-
genPadsParseS :: UString -> [LString] -> Maybe Pat -> Q [Dec]
genPadsParseS name args patM = do 
  { bodyS <- [| parseStringInput $(return foo) |]
  ; return [ FunD (mkTyParserSName name) [Clause parserArgs (NormalB bodyS) []] ]
  }
  where
    parserName = mkTyParserName name    
    parserArgs = map (mkVarParserName) args ++ pat
    parserArgsP = map VarP parserArgs
    foo = applyE (VarE (mkTyParserName name)) (map VarE parserArgs)
    pat = Maybe.maybeToList patM
-}

--------------------------------------------------------------
-- GENERATING PARSER DECLARATION FROM DATA DECLARATION
--------------------------------------------------------------

genPadsDataParseM :: UString -> [LString] -> (Maybe Pat) -> PadsData -> Q [Dec] 
genPadsDataParseM name args patM padsData = do 
  { body  <- genParseData padsData
  ; return [ FunD parser_name [Clause parserArgs (NormalB body) []] ]
  }
  where
    (parser_name,parserArgs) = mkParserNameArgs name args patM


--------------------------------------------------------------
-- GENERATING PARSER DECLARATION FROM NEWTYPE DECLARATION
--------------------------------------------------------------

genPadsNewParseM :: UString -> [LString] -> (Maybe Pat) -> BranchInfo -> Q [Dec] 
genPadsNewParseM name args patM branch = do 
  { (dec,exp) <- genParseBranchInfo branch
  ; let body = LetE [dec] exp
  ; return [ FunD parser_name [Clause parserArgs (NormalB body) []] ]
  }
  where
    (parser_name,parserArgs) = mkParserNameArgs name args patM


------------------------------------------------------
-- GENERATING PARSER FROM TYPE EXPRESSION
------------------------------------------------------

genParseTy :: PadsTy -> Q Exp
genParseTy (PConstrain pat ty exp)   = genParseConstrain pat ty exp
genParseTy (PTransform src dest exp) = genParseTyTrans src dest exp
genParseTy (PList ty sep term)       = genParseList ty sep term
genParseTy (PApp tys argE)           = genParseTyApp tys argE
genParseTy (PTuple tys)              = genParseTuple tys
genParseTy (PExpression exp)         = genParseExp exp
genParseTy (PTycon c)                = return $ mkParseTycon c
genParseTy (PTyvar v)                = return $ mkParseTyvar v

genParseConstrain :: Pat -> PadsTy -> Exp -> Q Exp
genParseConstrain pat ty exp = [| parseConstraint $(genParseTy ty) $pred |]
  where
    pred = return (LamE [pat, VarP (mkName "md")] exp)

genParseTyTrans :: PadsTy -> PadsTy -> Exp -> Q Exp
genParseTyTrans tySrc tyDest exp
  = [| parseTrans $(genParseTy tySrc) (fst $(return exp)) |]

genParseList :: PadsTy -> (Maybe PadsTy) -> (Maybe TermCond) -> Q Exp
genParseList ty sep term =
  case (sep,term) of 
    (Nothing,  Nothing)          -> [| parseListNoSepNoTerm $(genParseTy ty) |]
    (Just sep, Nothing)          -> [| parseListSepNoTerm $(genParseTy sep) $(genParseTy ty) |]
    (Nothing,  Just (LLen lenE)) -> [| parseListNoSepLength $(return lenE) $(genParseTy ty) |]
    (Just sep, Just (LLen lenE)) -> [| parseListSepLength $(genParseTy sep) $(return lenE) $(genParseTy ty) |]
    (Nothing,  Just (LTerm term))-> [| parseListNoSepTerm $(genParseTy term) $(genParseTy ty) |]
    (Just sep, Just (LTerm term))-> [| parseListSepTerm $(genParseTy sep) $(genParseTy term) $(genParseTy ty) |]

genParseTuple :: [PadsTy] -> Q Exp
genParseTuple tys = do
  { f_rep <- buildF_rep vars_frep
  ; f_md  <- buildF_md vars_fmd md_vars vars_frep
  ; body  <- foldl parseNext [| return ($(dyn "f_rep"),$(dyn "f_md")) |] tys
  ; return (LetE [f_rep,f_md] body)
  }
  where
    vars_fmd  = [ mkName ("x"++show n) | n <- [1 .. length tys]] 
    md_vars   = [ mkName ("m"++show n) | n <- [1 .. length tys]] 
    vars_frep = [v | (v,t) <- zip vars_fmd tys, hasRep t]

buildF_rep :: [Name] -> Q Dec
buildF_rep vars_frep = do
  { body <- tupleTH vars_frep
  ; return (FunD (mkName "f_rep") [Clause (map VarP vars_frep) (NormalB body) [] ])
  }

buildF_md :: [Name] -> [Name] -> [Name] -> Q Dec
buildF_md vars_fmd md_vars vars_frep = do
  { body <- [| ($(genMergeBaseMDs md_vars), $(tupleTH vars_frep)) |]
  ; return (FunD (mkName "f_md") [Clause (map VarP vars_fmd)
              (NormalB (LetE decls body)) []])
  }
  where
    decls = zipWith buildMDecl md_vars vars_fmd
    buildMDecl m f
      = ValD (VarP m) (NormalB (AppE (VarE 'get_md_header) (VarE f))) []

parseNext :: Q Exp -> PadsTy -> Q Exp
parseNext prog t
  | hasRep t  = [| $prog =@= $(genParseTy t) |]
  | otherwise = [| $prog =@  $(genParseTy t) |]

genMergeBaseMDs [e] = return (VarE e)
genMergeBaseMDs es  = [| mergeBaseMDs $(listTH es) |]


genParseExp :: Exp -> Q Exp
genParseExp (LitE (CharL c)) = [| charLit_parseM c |]
genParseExp (LitE (StringL s)) = [| strLit_parseM s |]
genParseExp _ = error "genParseExp: Not yet defined"

genParseTyApp :: [PadsTy] -> Maybe Exp -> Q Exp
genParseTyApp tys expM = do
  { fs <- mapM genParseTy tys
  ; return (foldl1 AppE (fs ++ Maybe.maybeToList expM))
  }

mkParseTycon :: String -> Exp
mkParseTycon "EOF" = VarE 'eof_parseM
mkParseTycon "EOR" = VarE 'eor_parseM
mkParseTycon c     = VarE (mkTyParserName c)

mkParseTyvar :: String -> Exp
mkParseTyvar v = VarE (mkVarParserName v)


----------------------------------------------------------
-- GENERATING PARSERS FROM UNION/SWITCH EXPRESSIONS
----------------------------------------------------------

genParseData :: PadsData -> Q Exp
genParseData (PUnion bs)       = genParseUnion bs
genParseData (PSwitch exp pbs) = genParseSwitch exp pbs

genParseUnion :: [BranchInfo] -> Q Exp
genParseUnion bs = do
  { (decs,bodies) <- fmap unzip $ mapM genParseBranchInfo bs
  ; let body = VarE 'choiceP `AppE` ListE bodies
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

genParseBranchInfo (BConstr c args pred) = do
  { body   <- foldl parseNext [| return ($conQ,$(return (VarE fnMD))) |] tys
  ; con_md <- genConstr_md fnMD conMD vars_conmd md_vars vars_con
  ; return (con_md, body)
  }
  where
    tys = [ty | (strict,ty) <- args]
    vars_conmd = [ mkName ("x"++show n) | n <- [1 .. length tys]] 
    md_vars    = [ mkName ("m"++show n) | n <- [1 .. length tys]] 
    vars_con   = [v | (v,t) <- zip vars_conmd tys, hasRep t]
    conQ  = return (ConE (mkConstrName c))
    conMD = ConE (mkConstrMDName c)
    fnMD  = mkfnMDName c

genParseBranchInfo (BRecord c fields pred) = do
  { body   <- foldl parseNext [| return ($conQ,$(return (VarE fnMD))) |] tys
  ; con_md <- genConstr_md fnMD conMD vars_conmd md_vars vars_con
  ; return (con_md, body)
  }
  where
    tys = [ty | (labelM, (strict,ty), expM) <- fields]
    vars_conmd = [ mkName ("x"++show n) | n <- [1 .. length tys]] 
    md_vars    = [ mkName ("m"++show n) | n <- [1 .. length tys]] 
    vars_con   = [v | (v,t) <- zip vars_conmd tys, hasRep t]
    conQ  = return (ConE (mkConstrName c))
    conMD = ConE (mkConstrMDName c)
    fnMD  = mkfnMDName c


genConstr_md :: Name -> Exp -> [Name] -> [Name] -> [Name] -> Q Dec
genConstr_md fnMD conMD vars_fmd md_vars vars_conmd = do
  { body <- [| ($(genMergeBaseMDs md_vars), $(return (applyE conMD (map VarE vars_conmd)))) |]
  ; return (FunD fnMD [Clause (map VarP vars_fmd)
              (NormalB (LetE decls body)) []])
  }
  where
    decls = zipWith buildMDecl md_vars vars_fmd
    buildMDecl m f
      = ValD (VarP m) (NormalB (AppE (VarE 'get_md_header) (VarE f))) []





    


------------------------------------------
-- PRINTING FUNCTIONS
------------------------------------------







------------------------------------
-- Name manipulation functions 
------------------------------------

-- Naming types, and accessing the names of types

mkRepName str = mkName str

mkMDName str = case M.lookup str baseTypesMap of
         Nothing -> mkName (str ++ "_md")
         Just _ -> ''Base_md         


mkIMDName name = mkName (name ++ "_imd")


mkRepNameTQ str = return (ConT (mkRepName str))
mkMDNameTQ str = return (ConT (mkMDName str))

mkMDVarName name = mkName (name ++ "_md")


-- Naming fields and constructors

mkFieldName str   = mkName str
mkFieldMDName str = mkName (str++"_md")

mkConstrName   str = mkName str
mkConstrMDName str = mkName (str++"_md")
mkfnMDName str     = mkName (strToLower str ++ "_md")


-- Naming Parsers

mkTyParserName  str = mkName ((strToLower str) ++ "_parseM")
mkTyParserSName str = mkName ((strToLower str) ++ "_parseS")

mkVarParserName str = mkName (str ++ "__parseM")


-- Naming Printers

mkPrintFLName str = mkName ((strToLower str) ++ "_printFL")


 
























































------------------------------------
-- Old Stuff 
------------------------------------



{-
getBranchMDNameU str = mkName ((strToUpper str)++"_md")
getBranchNameU str = mkName (strToUpper str)

getBranchMDNameL str = mkName ((strToLower str)++"_md")
getBranchNameL   str = mkName  (strToLower str)
-}

{-

genRepMDDeclStruct :: Name -> Name -> [(Maybe String, PadsTy, Maybe Exp)] -> (Dec, [Dec], Type)
genRepMDDeclStruct ty_name md_ty_name fields = 
  let (vsts', md_vsts') = unzip $ flattenMaybeList $ map genRepMDField fields
      derives      = [''Show, ''Eq, ''Typeable, ''Data, ''Ord]
      ty_con       = RecC ty_name vsts'
      ty_decl      = DataD [] ty_name [] [ty_con] derives
      inner_md_name = mkInnerMDName ty_name   -- ty name is the same as the declared pads type name
      imd_con       = RecC inner_md_name md_vsts'
      imd_decl      = DataD [] inner_md_name [] [imd_con] derives   -- declaration of line for nested components
      imd_ty        = ConT inner_md_name
      md_ty         = tyListToTupleTy [ConT ''Base_md, imd_ty]
      md_decl       = TySynD md_ty_name [] md_ty
  in if length vsts' == 0 then 
        error ("Error: Record " ++ (show ty_name) ++ " must contain at least one named field.")
     else 
        (ty_decl, [imd_decl,md_decl], md_ty)
 

mkPadsInstance parse_name print_name ty_name md_ty mpat_info = 
  let (inst, parsePP, printFL) = case mpat_info of
                          Nothing -> (AppT (AppT (ConT ''Pads) (ConT ty_name)) md_ty,   -- Pads RepTy MDTy
                                      mkName "parsePP",
                                      mkName "printFL")
                          Just (p,arg_ty) -> 
                                     (AppT 
                                        (AppT (AppT (ConT ''Pads1) arg_ty) (ConT ty_name)) 
                                        md_ty,   -- Pads Arg RepTy MDTy
                                      mkName "parsePP1",
                                      mkName "printFL1")
      parsePP_method = ValD (VarP parsePP) (NormalB (VarE parse_name)) []
      printFL_method = ValD (VarP printFL) (NormalB (VarE print_name)) []
  in [InstanceD [] inst [parsePP_method, printFL_method]]

genPadsParseS :: String -> Name -> Name -> Name -> PadsTy -> Maybe(Pat, Type) -> Q [Dec]
genPadsParseS p_name parse_name rep_name pd_name padsTy mpat_info = return [sigD, funD]
  where
       parseSName  = getParseSName p_name
       stringTy    = ConT ''String
       padsPairTy  = AppT (AppT (TupleT 2) (ConT rep_name)) (ConT pd_name)
       resultTy    = AppT (AppT (TupleT 2) padsPairTy) stringTy
       core_ty     = arrowTy stringTy resultTy
       (bodyE, ty) = case mpat_info of
                      Nothing -> (VarE 'parseS, core_ty)
                      Just (pat,pat_ty) -> (LamE [pat] (AppE (VarE 'parseS1) (patToExp pat)),
                                            arrowTy pat_ty core_ty)
       sigD = SigD parseSName ty
       funD = ValD (VarP parseSName) (NormalB bodyE ) []
   
-}


--   printFL :: [Dec]           <- genPadsPrintFL p_name print_name    ty_name md_ty_name padsTy arg_info_opt

{-
accumulator: String -> String
when apply to a string, it appends a string on the front of whatever you apply it to

let p = \x . "hello world" ++ x
let q = \y -> "holiday" ++ y

new accumulator is: p . q
left-linear tree of compositions
apply accumulator to end of string
associativity for composition starts building string up over one one pass.

define own datatype
 binary tree datatype w/strings at the leaves; want to keep it relatively balanced, an AVL tree (maybe in library)
 put strings on the end, then you could start putting strings 

pair of lists to encode a queue
-}

{-

genPadsPrintFL :: String -> Name ->    Name ->  Name -> PadsTy -> Maybe (Pat, Type) -> Q [Dec]
genPadsPrintFL    p_name    print_name rep_name pd_name padsTy mpat_info = do 
   core_bodyE <- printE rep_name pd_name padsTy
   let core_ty = arrowTy (AppT (AppT (TupleT 2) (ConT rep_name)) (ConT pd_name)) (ConT (mkName "FList"))
   let (bodyE,ty) = case mpat_info of
                     Nothing -> (core_bodyE, core_ty)
                     Just (pat,pat_ty) -> ( LamE [pat] core_bodyE,
                                            arrowTy pat_ty core_ty)
   let sigD = SigD print_name ty
   let funD = ValD (VarP print_name) (NormalB bodyE) []
   return [sigD, funD]

printE :: Name -> Name -> PadsTy -> Q Exp
printE repN mdN ty = do
   repName     <- genRepName 
   mdName      <- genMdName 
   let (repE,repP) = genPE repName
   let (mdE, mdP)  = genPE mdName
   let frepP       = wrapRepP repN ty repP 
   rhsE        <- printE' (ty, repE, mdE)
   let printFun = LamE [TupP [frepP,mdP]] rhsE
   return printFun


wrapRepP :: Name -> PadsTy -> Pat -> Pat
wrapRepP repN ty repP = ConP repN [repP]


printE' :: (PadsTy, Exp, Exp) -> Q Exp
printE' (ty, repE, mdE) = case ty of
  Plit  PS.EorL        -> return       (VarE(getPrintFLName "PeorLit"))                           
  Plit  PS.EofL        -> return       (VarE(getPrintFLName "PeofLit"))                           
  Plit  PS.VoidL       -> return       (VarE(getPrintFLName "PvoidLit"))            
  Plit  l              -> return (AppE (VarE 'litPrint) (litToExp l))
  Pname p_name   -> return (AppE  (VarE (getPrintFLName p_name))  (TupE [repE, mdE]))
  Ptuple ptys    -> printTuple ptys repE mdE 
  Pline ty'      -> printLine (ty', repE, mdE)
  Papp ty' argE  -> printApp (ty', argE, repE, mdE)
  Ptrans tySrc tyDst trans    -> printTrans(tySrc,tyDst,trans,repE,mdE)
  Ptypedef pat ty pred  -> printTypeDef (ty, repE, mdE)
  Precord recName fieldInfo -> printRecord (mkRepName recName) fieldInfo repE mdE
  Punion  unionName fieldInfo -> printUnion (mkRepName unionName) fieldInfo repE mdE
  Pmaybe ty -> printMaybe ty repE mdE
  Plist elemTy optSepTy optTermCond -> genPrintList elemTy optSepTy optTermCond repE mdE
  Ptry ty -> return (VarE('printNothing))
  Pswitch unionName whichE patBranches -> printUnion (mkRepName unionName) (map snd patBranches) repE mdE

genPrintList :: PadsTy -> (Maybe PadsTy) -> (Maybe TermCond) -> Exp -> Exp -> Q Exp
genPrintList ty sepOpt termCondOpt repE mdE = do 
  (elemRepE, elemRepP) <- doGenPE "elemrep"
  (elemMDE,  elemMDP)  <- doGenPE "elemmd"
  parseElemE <- printE' (ty, elemRepE, elemMDE)
  let parseElemFnE = LamE [TupP [elemRepP, elemMDP]] parseElemE
  sepElemE <- case sepOpt of 
                Nothing -> return (VarE 'printNothing)
                Just ty -> printE' (ty, TupE [], TupE [])
  termElemE <- case termCondOpt of
                Nothing -> return (VarE 'printNothing)
                Just (LengthTC _) -> return (VarE 'printNothing)
                Just (TyTC (Ptry _)) -> return (VarE 'printNothing)
                Just (TyTC (Ptuple [Ptry _])) -> return (VarE 'printNothing)
                Just (TyTC termTy) -> printE' (termTy, TupE [], TupE [])
  return (AppE (AppE (AppE (AppE (VarE 'printList) (TupE [repE, mdE])) parseElemFnE) sepElemE) termElemE)

printMaybe :: PadsTy -> Exp -> Exp -> Q Exp
printMaybe ty repE mdE = do 
  (jrepE, jrepP) <- doGenPE "rep"
  (jmdE,  jmdP)  <- doGenPE "md"
  justE <- printE' (ty, jrepE, jmdE)
  let justB = NormalB justE
  let jrepPat = ConP 'Just [jrepP]
  let jmdPat =  TupP[WildP, ConP 'Just [jmdP]]
  let justPat = TupP [jrepPat, jmdPat]
  let justmatch = Match justPat justB []
  let nrepPat = ConP 'Nothing []
  let nmdPat =  TupP[WildP, ConP 'Nothing []]
  let nPat = TupP [nrepPat, nmdPat]
  let nmatch = Match nPat (NormalB (VarE 'printNothing)) []
  let caseE = CaseE(TupE[repE,mdE]) [justmatch,nmatch]
  return caseE


printUnion :: Name -> [FieldInfo] -> Exp -> Exp -> Q Exp
printUnion ty_name branches repE mdE = do
  matches <- printBranches branches
  let caseE = CaseE (TupE [repE,mdE]) matches
  return caseE

printBranches :: [FieldInfo] -> Q [Match]
printBranches branches = mapM printBranch branches

printBranch :: (Maybe String, PadsTy, Maybe Exp) -> Q Match
printBranch (Just str, branchTy, pred) = do
  (repE, repP) <- doGenPE "rep"
  (mdE,  mdP)  <- doGenPE "md"
  bodyE <- printE' (branchTy, repE, mdE)
  let caseBodyB = NormalB bodyE
  let repPatArg = case branchTy of {Plit _ -> [] ; _ -> [repP]}
  let repPat = ConP (getBranchNameU str) repPatArg
  let mdPat =  TupP[WildP, ConP (getBranchMDNameU str) [mdP]]
  let casePat = TupP [repPat, mdPat]
  let match = Match casePat caseBodyB []
  return match


printTypeDef :: (PadsTy, Exp, Exp) -> Q Exp
printTypeDef (pty, repE, mdE) = printE' (pty, repE, AppE (VarE 'snd) mdE)

printTrans :: (PadsTy, PadsTy, Exp, Exp, Exp) -> Q Exp
printTrans (tySrc, tyDst, transE, repE, mdE) = do 
  (tmpRepE, tmpRepP) <- doGenPE "rep"
  (tmpMdE,  tmpMdP) <- doGenPE "md"
  let toDiskE = AppE (VarE 'snd) transE
  let    cvtE = AppE toDiskE (TupE [repE, mdE])
  printBodyE <- printE' (tySrc, tmpRepE, tmpMdE)
  let letPat = ValD (TupP [tmpRepP, tmpMdP]) (NormalB cvtE) []
  return (LetE [letPat] printBodyE)


printApp :: (PadsTy, Exp, Exp, Exp) -> Q Exp
printApp (ty, argE, repE, mdE) = case ty of
  Pname p_name   -> return (AppE (AppE  (VarE (getPrintFLName p_name))  argE) (TupE [repE, mdE]))

printLine :: (PadsTy, Exp, Exp) -> Q Exp
printLine trm = do
  elemE <- printE' trm
  return (AppE (VarE 'endRecord) elemE)

printRecord :: Name -> [FieldInfo] -> Exp -> Exp -> Q Exp
printRecord recName fields repE mdE = do 
  let (repEs, repPs) = getPEforFields getBranchNameL fields
  let (mdEs,  mdPs)  = getPEforFields getBranchMDNameL fields
  let ptys = map (\(n,ty,p) -> ty) fields
  let ty_rep_mds = zip3 ptys repEs mdEs
  expE <- mapM printE' ty_rep_mds
  let printItemsE = ListE expE
  let caseBody = NormalB (AppE (VarE 'concatFL) printItemsE)
  let mdPat  = TupP[WildP, RecP (mkInnerMDName recName) mdPs]
  let repPat = RecP recName repPs
  let casePat = TupP [repPat, mdPat]
  let match = Match casePat caseBody []
  let caseE = CaseE (TupE [repE,mdE]) [match]
  return caseE

getPEforField :: (String -> Name) -> (Maybe String, PadsTy, Maybe Exp) -> (Exp, Maybe FieldPat)
getPEforField mkFieldNm (nameOpt, pty, optPred) = case nameOpt of
  Nothing -> (TupE [], Nothing) 
  Just str -> let (varE, varP) = genPE (mkFieldNm str)
              in (varE, Just (mkFieldNm str, varP))

getPEforFields :: (String -> Name) -> [(Maybe String, PadsTy, Maybe Exp)] ->  ([Exp], [FieldPat])
getPEforFields mkFieldNm fields = 
  let eps =  map (getPEforField mkFieldNm) fields
      (es, pOpts) = List.unzip eps
      ps = Maybe.catMaybes pOpts
  in (es, ps)

printTuple :: [PadsTy] -> Exp -> Exp -> Q Exp
printTuple ptys repE mdE = do
  (repEs, repPs ) <- genPEforTuple ptys
  (mdEs,  mdPs ) <- genPEforTuple ptys
  let ty_rep_mds = zip3 ptys repEs mdEs
  expE <- mapM printE' ty_rep_mds
  let printItemsE = ListE expE
  let caseBody = NormalB (AppE (VarE 'concatFL) printItemsE)
  let mdPat = TupP [WildP, (TupP mdPs)]
  let repPat = TupP repPs
  let casePat = TupP [repPat, mdPat]
  let match = Match casePat caseBody []
  let caseE = CaseE (TupE [repE,mdE]) [match]
  return caseE

genPEforTy :: PadsTy -> Q (Exp, Maybe Pat)
genPEforTy pty = case pty of
  Plit _ -> return (TupE [], Nothing)
  _ -> do (varE, varP) <- doGenPE "var"
          return (varE, Just varP)

genPEforTuple :: [PadsTy] -> Q ([Exp], [Pat])
genPEforTuple tys = do 
  eps <- mapM genPEforTy tys
  let (es, pOpts) = List.unzip eps
  let ps = Maybe.catMaybes pOpts
  return (es, ps)


-}

{-


mkParseSwitch :: String -> Exp -> [(Pat, (Maybe String, PadsTy, Maybe Exp))] -> Q Exp
mkParseSwitch str testE pat_branches = let
  (pats, branches) = unzip pat_branches
  in do parseEs <- mkParseBranches str branches
        let pat_parses = zip pats parseEs
        let matches = map (\(pat,exp) -> Match pat (NormalB exp) []) pat_parses
        return (CaseE testE matches)

mkParseUnion :: String -> [(Maybe String, PadsTy, Maybe Exp)] -> Q Exp
mkParseUnion str branches = do
  parseEs     <- mkParseBranches str branches   
  return (AppE (VarE 'choiceP) (ListE parseEs))       -- choiceP [parse1, ..., parsen]



mkParseBranches :: String -> [(Maybe String, PadsTy, Maybe Exp)] -> Q [Exp]
mkParseBranches str branches = mapM (mkParseBranch str) branches

mkParseBranch :: String -> (Maybe String, PadsTy, Maybe Exp) -> Q Exp
mkParseBranch str (Nothing, padsTy, predM) = error ("Union ("++ str ++ ") branch is missing a name.")
mkParseBranch str (Just name, padsTy, predM) = do
   let repName  = getBranchNameL   name
   let mdName   = getBranchMDNameL name
   bmdName1     <- genBMdName 
   bmdName2     <- genBMdName 
   let (repE,  repP)  = genPE repName
   let ( mdE,  mdP)   = genPE mdName
   let (bmd1E, bmd1P) = genPE bmdName1
   let (bmd2E, bmd2P) = genPE bmdName2
   rhsE        <- genParseTy padsTy
   case (predM,padsTy) of
    (Just pred, Plit l) -> error ("Union "++ str ++ ": literal branch can't have a predicate.")
    (Nothing,   Plit l) -> let
       stmtPrs = BindS (TupP [repP,mdP]) rhsE                                     -- (rep, md) <- parse
       frepE   = ConE (getBranchNameU   name)                                  -- . inject value into data type: Foo 
       imdE    = AppE (ConE (getBranchMDNameU name))  mdE                   -- . inject md into data type: Foo_md md
       fmdE    = TupE [mdE,  imdE]                                                -- . build final md: (md, Foo_md md)
       resultE = TupE [frepE,fmdE]                                                -- . build final result: (Foo, (md, Foo_md md))
       stmtRet = NoBindS (AppE (VarE 'mdReturn) resultE)                            -- return (Foo, (md, Foo_md md))
       in return (DoE [stmtPrs,stmtRet])
    (Nothing, _) -> let
       stmtPrs = BindS (TupP [repP,mdP]) rhsE                                     -- (rep, md) <- parse
       stmtGmd = LetS [ValD bmd1P (NormalB (AppE (VarE 'get_md_header) mdE)) []]  -- let mbd1 = get_md_header md
       frepE   = AppE (ConE (getBranchNameU   name)) repE                   -- . inject value into data type: Foo rep
       imdE    = AppE (ConE (getBranchMDNameU name))  mdE                   -- . inject md into data type: Foo_md md
       fmdE    = TupE [bmd1E,imdE]                                                -- . build final md: (bmd1, Foo_md md)
       resultE = TupE [frepE,fmdE]                                                -- . build final result: 
       stmtRet = NoBindS (AppE (VarE 'mdReturn) resultE)                            -- return (Foo rep, (bmd1, Foo_md md))
       in return (DoE [stmtPrs,stmtGmd,stmtRet])
    (Just pred,_) -> let
       stmtPrs = BindS (TupP [repP,mdP]) rhsE                                     -- (rep,md) <- parse
       stmtGmd = LetS [ValD bmd1P (NormalB (AppE (VarE 'get_md_header) mdE)) []]  -- let mbd1 = get_md_header md
       predTestE = CondE pred bmd1E (AppE (VarE 'addPredFailureMD) bmd1E)      -- . build predicate test 
       stmtPred  = LetS [ValD bmd2P (NormalB predTestE)  []]                      -- let mbd2 = if pred then bmd1 else addPredFailureMD bmd1
       frepE   = AppE (ConE (getBranchNameU   name)) repE                   -- . inject value into data type: Foo rep
       imdE    = AppE (ConE (getBranchMDNameU name))  mdE                   -- . inject md into data type:    Foo_md md
       fmdE    = TupE [bmd2E,imdE]                                                -- . build final md:              (mbd2, Foo_md md)
       resultE = TupE [frepE,fmdE]                                                -- . build final result           (Foo rep, (md2, Foo_md md))
       stmtRet = NoBindS (AppE (VarE 'mdReturn) resultE)                            -- return (Foo rep, (md2, Foo_md md))
       in return (DoE [stmtPrs,stmtGmd,stmtPred,stmtRet])                      

mkParseRecord :: String -> [(Maybe String, PadsTy, Maybe Exp)] -> Q Exp
mkParseRecord str fields = do
  (repEs,mdEs,bmdEs, stmts) <- mkParseFields fields
  let tyName             = mkName str
  let top_md             = mkName "top_md"
  let (top_mdE, top_mdP) = genPE top_md
  let headerE            = AppE (VarE 'mergeBaseMDs) (ListE bmdEs)
  let mdS                = LetS [ValD top_mdP (NormalB headerE) []]
  let repE               = RecConE tyName repEs
  let inner_md_name      = mkInnerMDName tyName   -- ty name is the same as the declared pads type name
  let mdE                = TupE [top_mdE, RecConE inner_md_name mdEs]
  let resultE            = TupE [repE,mdE]
  let finalS             = NoBindS (AppE (VarE 'return) resultE)
  return (DoE (stmts ++ [mdS,finalS]))



mkParseField :: (Maybe String, PadsTy, Maybe Exp) -> Q ([FieldExp], [FieldExp], Exp, [Stmt])
mkParseField (labelM, ty, predM) = do
   repName     <- case labelM of { Nothing -> genRepName; Just str -> return $ mkFieldName   str}
   mdName      <- case labelM of { Nothing -> genMdName;  Just str -> return $ mkFieldMDName str}
   bmdName     <- genBMdName 
   let (repE, repP) = genPE repName
   let ( mdE,  mdP) = genPE mdName
   let (bmdE, bmdP) = genPE bmdName
   rhsE        <- genParseTy ty
   case (labelM,predM) of 
    (Nothing, Just p)      ->  error "Predicates cannot modify unnamed fields in records."
    (Nothing, Nothing)      ->  let                                         -- Parse unnamed, non-literal struct field
       stmt1 = BindS (TupP [repP,mdP]) rhsE                                    -- rep and md exist for non-literal types
       stmt2 = LetS [ValD bmdP (NormalB (AppE (VarE 'get_md_header) mdE)) []]  -- Read out header of resulting parse descriptor
       in return([], [], bmdE, [stmt1,stmt2])                                  -- No rep or md to include in result
    (Just str, Nothing)    -> let                                          -- Parse named, non-literal struct field, no predicate
       stmt1 = BindS (TupP [repP,mdP]) rhsE                                    -- rep and md exist for non-literal types
       stmt2 = LetS [ValD bmdP (NormalB (AppE (VarE 'get_md_header) mdE)) []]  -- Read out header of resulting parse descriptor
       in return([(repName,repE)], [(mdName,mdE)], bmdE, [stmt1,stmt2])       -- Include named rep and md in result
    (Just str, Just pred)    -> do                                         -- Parse named, non-literal struct field, predicate
      final_mdName    <- genMdName
      raw_bmdName     <- genBMdName 
      let (finalMDE, finalMDP)   = genPE final_mdName
      let (rawBMDE, rawBMDP) = genPE raw_bmdName
      let predTestE      = CondE pred rawBMDE (AppE (VarE 'addPredFailureMD) rawBMDE)    -- if pred then rawBMD else addPredFailureMD rawBMD
      let replaceHeaderE = AppE (AppE (VarE 'replace_md_header) mdE) bmdE                -- replace_md_header rawMD bmd
      let stmt1 = BindS (TupP [repP,mdP]) rhsE
      let stmt2 = LetS [ValD rawBMDP (NormalB (AppE (VarE 'get_md_header) mdE)) []]
      let stmt3 = LetS [ValD bmdP (NormalB predTestE)  []] 
      let stmt4 = LetS [ValD finalMDP  (NormalB replaceHeaderE) []]
      return ([(repName,repE)], [(mdName,finalMDE)], bmdE, [stmt1,stmt2,stmt3,stmt4])       -- Include named rep and md in result

mkParseFields :: [(Maybe String, PadsTy, Maybe Exp)] -> Q ([FieldExp], [FieldExp], [Exp], [Stmt])
mkParseFields [] = return ([],[],[],[])
mkParseFields (field:fields) = do
  (rep_field,   md_field,  bmd_field,  stmts_field)  <- mkParseField  field
  (reps_fields, md_fields, bmd_fields, stmts_fields) <- mkParseFields fields
  return (rep_field++reps_fields, md_field++md_fields, bmd_field:bmd_fields, stmts_field++stmts_fields)

addPredFailureMD :: Base_md -> Base_md
addPredFailureMD (Base_md{numErrors, errInfo}) = 
  let errInfo' = case errInfo of
                  Nothing -> E.ErrInfo {msg = E.FPredicateFailure, position = Nothing}
                  Just e ->  e
  in Base_md{numErrors = numErrors + 1, errInfo = Just errInfo'}
       
-}       



{- 
Invariants: literal can't have a field name or a predicate; no field name, no predicate
   stmts to parse each field of a record
   do
    (field_name, field_name_raw_md) <- parse_1             -- if field_name exists, not a literal, predicate
    let raw_bmd_1 = get_md_header field_name_raw_md
    let bmd_1 = if pred_1 then raw_bmd_1
                else addPredFailureMD raw_bmd_1
    let field_name_md = replace_md_header field_name_raw_md bmd_1
    ...
    (field_name, field_name_md) <- parse_field_name        -- if field_name exists, not a literal, no predicate
    let bmd_field_name = get_md_header field_name_md
    ... 
    md_i <- parse_i                                        -- no field name, literal field, no predicate
    let bmd_i = get_md_header md_i
    ...
    (rep_j,md_j) <- parse_j                                -- no field name, not literal, no predicate
    let bmd_j = get_md_header md_j

    let top_md = mergeBaseMDs [bmd_1,...bmd_n]
    let name_md = Name_md{name_1 = field_name_md, ... }
    return (rep,(top_md,name_md))
-}



{-

-- Type declaration for parsers 

    ty = abstract result_ty [ ConT ''PadsParser `AppT` am | am <- argsTMDT ]
    result_ty = ConT ''PadsParser `AppT` apply (TupleT 2) [rep, md]
    
    rep,md :: Type
    rep = apply (ConT (mkRepName name)) argsT
    md  = apply (ConT (mkMDName name)) argsMDT
        
    argsT,argsMDT :: [Type]
    argsT   = [VarT (mkName a)      | a <- args]
    argsMDT = [VarT (mkMDVarName a) | a <- args]
    argsTMDT= [apply (TupleT 2) [a,m]  | (a,m) <- zip argsT argsMDT]
    
    contextTy = ForallT freeVars [ClassP ''PadsMD [m] | m <- argsMDT ] ty
    freeVars = map PlainTV (map mkName args ++ map mkMDVarName args)
    
-}
