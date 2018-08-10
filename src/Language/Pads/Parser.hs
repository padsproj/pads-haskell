{-# OPTIONS_HADDOCK prune #-}
{-|
  Module      : Language.Pads.Parser
  Description : Parser for the syntax of Pads
  Copyright   : (c) 2011
                Kathleen Fisher <kathleen.fisher@gmail.com>
                John Launchbury <john.launchbury@gmail.com>
  License     : MIT
  Maintainer  : Karl Cronburg <karl@cs.tufts.edu>
  Stability   : experimental

  This module implements the parser for the PADS syntax in Haskell using parser
  combinators.
-}
module Language.Pads.Parser where

import Language.Pads.Syntax


import Text.Parsec hiding (upper,lower)
import qualified Text.Parsec.String as PS
import Text.Parsec.Error
import Text.Parsec.Prim as PP
import qualified Text.Parsec.Token as PT
import Text.Parsec.Language
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Pos
import Text.Parsec.Expr
import Control.Monad

import qualified Language.Haskell.Meta as LHM
import Language.Haskell.TH

import Data.Char
--import System.FilePath.Glob



type Parser = PS.Parser
type Env    = [String]

-- | The main entry point for the Pads QuasiQuoter
parsePadsDecls :: SourceName -> Line -> Column -> String -> Either ParseError [PadsDecl]
parsePadsDecls fileName line column input
  = PP.parse (do { setPosition (newPos fileName line column)
                 ; whiteSpace
                 ; x <- padsDecls
                 ; whiteSpace
                 ; eof <|> errorParse
                 ; return x
                 }) fileName input

errorParse = do
  { rest <- manyTill anyToken eof
  ; unexpected rest }

-------------------------------------------------------------------------------
-- * PADS DECLARATIONS

padsDecls :: Parser [PadsDecl]
padsDecls = option [] (many1 topDecl)

topDecl :: Parser PadsDecl
topDecl
  =  typeDecl <|> dataDecl <|> newDecl <|> obtainDecl
 <?> "Pads declaration keyword"

typeDecl :: Parser PadsDecl
typeDecl
  = do { reserved "type"
       ; (id,env) <- declLHS; pat <- patLHS
       ; rhs <- ptype env
       ; genM <- optionMaybe gen
       ; return (PadsDeclType id env pat rhs genM)
       } <?> "Pads type declaration"

dataDecl :: Parser PadsDecl
dataDecl
  = do { reserved "data"
       ; (id,env) <- declLHS; pat <- patLHS
       ; rhs <- dataRHS env; drvs <- option [] derives
       ; return (PadsDeclData id env pat rhs drvs)
       } <?> "Pads data declaration"

newDecl :: Parser PadsDecl
newDecl
  = do { reserved "newtype"
       ; (id,env) <- declLHS; pat <- patLHS
       ; rhs <- newRHS env; drvs <- option [] derives
       ; return (PadsDeclNew id env pat rhs drvs)
       } <?> "Pads newtype declaration"

obtainDecl :: Parser PadsDecl
obtainDecl
  = do { reserved "obtain"
       ; (id,env) <- declLHS
       ; reservedOp "from"; rhs <- ptype env
       ; reserved "using"; exp <- expression
       ; genM <- optionMaybe gen
       ; return (PadsDeclObtain id env rhs exp genM)
       } <?> "Pads transform type"

declLHS
  = do { id <- upper; env <- try $ many lower
       ; return (id,env)
       }

patLHS
  = do { p <- try $ haskellParsePatTill "="
       ; return (Just p)
       }
    <|> (reservedOp "=" >> return Nothing)

derives
  = reserved "deriving" >>
    (do { q <- qualUpper; return [q] }
    <|> parens (commaSep1 qualUpper))

-------------------------------------------------------------------------------
-- * PADS TYPES

ptype :: Env -> Parser PadsTy
ptype env
  =  constrain env
 <|> obtain env
 <|> partition env
 <|> listTy env
 <|> value env
 <|> btype env
 <?> "Pads Pads type expression"

constrain :: Env -> Parser PadsTy
constrain env
  = do { reserved "constrain"
       ; pat <- haskellParsePatTill "::"; ty <- ptype env
       ; exp <- predic
       ; return (PConstrain pat ty exp)
       } <?> "Pads constrain type"

predic = do { reservedOp "where"; expression }
gen    = do { reservedOp "generator"; expression }

obtain :: Env -> Parser PadsTy
obtain env
  = do { reserved "obtain"; dst <- ptype env
       ; reservedOp "from"; src <- ptype env
       ; reserved "using"; exp <- expression
       ; genM <- optionMaybe gen
       ; return (PTransform src dst exp genM)
       } <?> "Pads transform type"

partition :: Env -> Parser PadsTy
partition env
  = do { reserved "partition"; ty <- ptype env
       ; reserved "using"; exp <- expression
       ; return (PPartition ty exp)
       } <?> "Pads partition type"

listTy :: Env -> Parser PadsTy
listTy env
  = do { (elm,sepM) <- brackets (listInside env)
       ; termM <- listEnd env
       ; return (PList elm sepM termM)
       } <?> "Pads list type"

listInside env
  = do { elm <- ptype env
       ; sepM <- optionMaybe (reservedOp "|" >> ptype env)
       ; return (elm,sepM)
       }

listEnd env
  = optionMaybe
    (  do {reservedOp "terminator"; t<-ptype env; return (LTerm t)}
   <|> do {reservedOp "length"; e<-expression; return (LLen e)})

value env
  = do { reserved "value"
       ; exp <- expression; reservedOp "::"
       ; ty <- ptype env
       ; return (PValue exp ty)
       }


btype :: Env -> Parser PadsTy
btype env
  = try $ do
       { ty <- etype env; tys <- many (atype env)
       ; expM <- optionMaybe (try expression);
       ; if length tys==0 && expM == Nothing
         then return ty
         else return (PApp (ty:tys) expM) }

etype :: Env -> Parser PadsTy
etype env = atype env
         <|> try (expression >>= (return . PExpression))

atype env
  =  try (tuple env)
 <|> do { (elm,sepM) <- brackets (listInside env)
        ; return (PList elm sepM Nothing)}
 <|> fmap PTycon qualUpper
 <|> fmap PTyvar (tyvar env)

tuple :: Env -> Parser PadsTy
tuple env
  = do { tys <- parens $ option [] (commaSep1 (ptype env))
       ; case length tys of
           0 -> return (PTycon ["Void"])
           1 -> return (head tys)
           _ -> return (PTuple tys)
       }
  <?> "Pads tuple type"

-------------------------------------------------------------------------------
-- * PADS DATA DECLARATIONS

dataRHS :: Env -> Parser PadsData
dataRHS env
  =  switchTy env
 <|> fmap PUnion (constrs env)
 <?> "Pads data type right hand side"

switchTy :: Env -> Parser PadsData
switchTy env
  = do { reservedOp "case"; exp <- expression
       ; reservedOp "of"; brs <- branch env `sepBy1` reservedOp "|"
       ; return (PSwitch exp brs)
       } <?> "Pads switch type"

branch :: Env -> Parser (Pat, BranchInfo)
branch env
  = do { pat <- haskellParsePatTill "->"; br <- constr env
       ; return (pat, br)
       } <?> "Pads switch branch"

constrs :: Env -> Parser [BranchInfo]
constrs env = constr env `sepBy1` reservedOp "|"

constr :: Env -> Parser BranchInfo
constr env
  = do { id  <- upper;
       ; do { args <- record env; predM <- optionMaybe predic
            ; return (BRecord id args predM)}
     <|> do { args <- option (mkId id) (constrArgs env)
            ; predM <- optionMaybe predic
            ; return (BConstr id args predM)}}
  where
    mkId id = [(NotStrict, PExpression (LitE (StringL id)))]
              -- Provides the expansion e.g.: Tue -> Tue "Tue"


constrArgs :: Env -> Parser [ConstrArg]
constrArgs env
  = many1 $ do
    { bang <- option NotStrict (reservedOp "!" >> return IsStrict)
    ; ty <- etype env
    ; return (bang,ty)
    }


record :: Env -> Parser [FieldInfo]
record env
  = do { reservedOp "{"
       ; flds <- field env `sepBy` reservedOp ","
       ; reservedOp "}"
       ; return flds
       } <?> "Pads record type"

field :: Env -> Parser FieldInfo
field env
  =  try (do { id <- (lower << reservedOp "::")
        ; ty <- ftype env
        ; predM <- optionMaybe predic
        ; genM  <- optionMaybe gen
        ; return (Just id, ty, predM, genM)
        })
 <|> try (do { id <- lower; reservedOp "="
        ; reserved "value"
        ; exp <- expression; reservedOp "::"
        ; (strict,ty) <- ftype env
        ; predM <- optionMaybe predic
        ; genM <- optionMaybe gen
        ; return (Just id, (strict, PValue exp ty), predM, genM)
        })
 <|> do { ty <- ftype env
        ; let recordid =  (case ty of
                            (strict,PConstrain (VarP name) _ _) -> Just $ nameBase name
                            otherwise -> Nothing
                          )
        ; predM <- optionMaybe predic
        ; genM <- optionMaybe gen
        ; return (recordid, ty, predM, genM)
        }
 <?>  "record field"

ftype env
  =  do { reservedOp "!"; ty <- atype env; return (IsStrict,ty)}
 <|> do { ty <- ptype env; return (NotStrict,ty)}

-------------------------------------------------------------------------------
-- * PADS NEW TYPE DECLARATIONS

newRHS :: Env -> Parser BranchInfo
newRHS env
  = do { id  <- upper;
       ; do { rec <- record1 env
            ; predM <- optionMaybe predic
            ; return (BRecord id rec predM)}
     <|> do { arg <- atype env
            ; predM <- optionMaybe predic
            ; return (BConstr id [(NotStrict,arg)] predM)
            }
       }

record1 :: Env -> Parser [FieldInfo]
record1 env
  = do { reservedOp "{"
       ; args1 <- many (ftype env << reservedOp ",")
       ; fld <- field1 env
       ; args2 <- many (reservedOp "," >> ftype env)
       ; reservedOp "}"
       ; return (map expand args1 ++ [fld] ++ map expand args2)
       } <?> "Pads newtype record"
  where
    expand fty = (Nothing, fty, Nothing, Nothing)

field1 :: Env -> Parser FieldInfo
field1 env
  = do { id <- lower; reservedOp "::"; ty <- ptype env
       ; predM <- optionMaybe predic
       ; genM <- optionMaybe gen
       ; return (Just id, (NotStrict,ty), predM, genM)
       }

-------------------------------------------------------------------------------
-- * HASKELL IN PADS DECLARATIONS

expression :: Parser Exp
expression =  haskellExp
          <|> literal

haskellExp :: Parser Exp
haskellExp = do { reservedOp "<|"
                ; haskellParseExpTill "|>"
                }
          <?> "Pads Haskell expression"


haskellParseExp :: String -> Parser Exp
haskellParseExp str = case LHM.parseExp str of
                        Left err    -> parserZero
                        Right expTH -> return expTH

haskellParseExpTill :: String -> Parser Exp
haskellParseExpTill op = do { str <- manyTill anyChar (reservedOp op)
                            ; haskellParseExp str
                            }

haskellParsePat :: String -> Parser Pat
haskellParsePat str = case LHM.parsePat str of
                        Left err    -> parserZero
                        Right patTH -> return patTH

haskellParsePatTill :: String -> Parser Pat
haskellParsePatTill op = do { str <- manyTill anyChar (reservedOp op)
                            ; haskellParsePat str
                            }


literal :: Parser Exp
literal =  fmap (LitE . CharL) (try charLiteral)
       <|> reLiteral
       <|> fmap (LitE . StringL) stringLiteral
       <|> fmap (LitE . IntegerL) (try integer)
       <|> fmap (VarE . mkName . qName) qualLower
       <|> fmap (ConE . mkName . qName) qualUpper
       <?> "Pads literal"

reLiteral :: Parser Exp
reLiteral = do { reservedOp reMark
               ; str <- manyTill anyChar (reservedOp reMark)
               ; return (ConE (mkName "RE") `AppE` LitE (StringL str))
               }
reMark = "'"

literalPat :: Parser Pat
literalPat =  fmap (LitP . CharL) (try charLiteral)
       <|> reLiteralPat
       <|> fmap (LitP . StringL) stringLiteral
       <|> fmap (LitP . IntegerL) (try integer)
       <|> fmap (VarP . mkName . qName) qualLower
       <|> fmap (flip ConP [] . mkName . qName) qualUpper
       <?> "Pads literal"

reLiteralPat :: Parser Pat
reLiteralPat = do { reservedOp reMark
               ; str <- manyTill anyChar (reservedOp reMark)
               ; return (ConP (mkName "RE") [LitP (StringL str)])
               }

qualUpper, qualLower :: Parser QString
qualUpper = try (upper `sepBy1` reservedOp ".")
qualLower = try $ do { prefix <- many (upper << reservedOp ".")
                     ; final <- lower
                     ; return (prefix ++ [final])
                     }

upper :: Parser String
upper = try $ do { id <- identifier
                 ; guard $ isUpper (head id)
                 ; return id}

lower :: Parser String
lower = try $ do { id <- identifier
                 ; guard $ isLower (head id)
                 ; return id}

tyvar env = try $ do { v <- lower
                     ; guard (v `elem` env)
                     ; return v }

-------------------------------------------------------------------------------
-- * LEXER

p << q = do {x<-p;q;return x}


lexer :: PT.TokenParser ()
lexer = PT.makeTokenParser (haskellStyle
  { reservedOpNames = ["=", "=>", "{", "}", "::", "<|", "|>", "|", reMark, "." ],
    reservedNames   = ["data", "type", "newtype", "old", "existing", "deriving",
                       "using", "where", "terminator", "length", "of", "from",
                       "case", "constrain", "obtain", "partition","value","generator" ]})

whiteSpace    = PT.whiteSpace  lexer
identifier    = PT.identifier  lexer
operator      = PT.operator    lexer
reserved      = PT.reserved    lexer
reservedOp    = PT.reservedOp  lexer
charLiteral   = PT.charLiteral lexer
stringLiteral = PT.stringLiteral  lexer
integer       = PT.integer     lexer
commaSep1     = PT.commaSep1   lexer
parens        = PT.parens      lexer
braces        = PT.braces      lexer
brackets      = PT.brackets    lexer
