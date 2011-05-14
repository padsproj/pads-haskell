module Language.Pads.Parser where 

{-
** *********************************************************************
*                                                                      *
*         (c)  Kathleen Fisher <kathleen.fisher@gmail.com>             *
*              John Launchbury <john.launchbury@gmail.com>             *
*                                                                      *
************************************************************************
-}


-- This is the parser for the PADS syntax in Haskell

import Language.Pads.Syntax


import Text.Parsec
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
import System.FilePath.Glob



type Parser = PS.Parser
type Env    = [UString]

-- The main entry point for the QuasiQuoter is parsePadsDecls.


parsePadsDecls :: SourceName -> Line -> Column -> String -> Either ParseError [PadsDecl]
parsePadsDecls fileName line column input 
  = PP.parse (do { setPosition (newPos fileName line column)
                 ; whiteSpace
                 ; x <- padsDecls
                 ; eof
                 ; return x
                 }) fileName input




lexer :: PT.TokenParser ()
lexer = PT.makeTokenParser (haskellStyle 
             { reservedOpNames = ["=", "=>", "{", "}", "::", "<|", "|>", "|", reMark ],
               reservedNames   = ["data", "type", "newtype", "old", "existing", "deriving",
                                   "using", "where", "terminator", "length", "of", "from",
                                   "case", "constrain", "obtain", "partition" ]})

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





-------------------------
-- PADS syntax parsing
-------------------------



padsDecls :: Parser [PadsDecl]
padsDecls = option [] (many1 topDecl)

topDecl :: Parser PadsDecl
topDecl 
  =  typeDecl <|> dataDecl <|> newDecl
 <?> "Pads declaration keyword"

typeDecl :: Parser PadsDecl
typeDecl 
  = do { old <- ((reserved "type" >> return False)
            <|> try (reserved "old" >> reserved "type" >> return True))
       ; (id,env,pat) <- declLHS
       ; rhs <- ptype env
       ; return (PadsDeclType old id env pat rhs)
       } <?> "Pads type declaration"

dataDecl :: Parser PadsDecl
dataDecl 
  = do { old <- (reserved "data" >> return False)
            <|> (reserved "old" >> reserved "data" >> return True)
       ; (id,env,pat) <- declLHS
       ; rhs <- dataRHS env; drvs <- option [] derives
       ; return (PadsDeclData old id env pat rhs drvs)
       } <?> "Pads data declaration"

newDecl :: Parser PadsDecl
newDecl 
  = do { reserved "newtype"; (id,env,pat) <- declLHS
       ; rhs <- newRHS env; drvs <- option [] derives
       ; return (PadsDeclNew False id env pat rhs drvs)
       } <?> "Pads newtype declaration"

declLHS
  = do { id <- upperId; env <- option [] (try $ many var)
       ; pat <- patLHS
       ; return (id,env,pat)
       }

patLHS
  = do { p <- try $ haskellParsePatTill "="
       ; return (Just p)
       }
    <|> (reservedOp "=" >> return Nothing)

derives 
  = reserved "deriving" >> 
    (do { q <- qtycl; return [q] }
	<|> parens (commaSep1 qtycl))


-------------------------

ptype :: Env -> Parser PadsTy
ptype env 
  =  constrain env
 <|> transform env
 <|> partition env
 <|> listTy env
 <|> btype env
 <?> "Pads Pads type expression"

constrain :: Env -> Parser PadsTy
constrain env
  = do { reserved "constrain"
       ; pat <- haskellParsePatTill "::"; ty <- ptype env
       ; reserved "where"; exp <- expression
       ; return (PConstrain pat ty exp)
       } <?> "Pads constrain type"


transform :: Env -> Parser PadsTy
transform env
  = do { reserved "obtain"; dst <- ptype env
       ; reservedOp "from"; src <- ptype env
       ; reserved "using"; exp <- expression 
       ; return (PTransform src dst exp)
       } <?> "Pads transform type"


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

partition :: Env -> Parser PadsTy
partition env
  = do { reserved "partition"; ty <- ptype env
       ; reserved "using"; exp <- expression 
       ; return (PPartition ty exp)
       } <?> "Pads partition type"


btype :: Env -> Parser PadsTy
btype env
  = do { ty <- etype env; tys <- many (atype env)
       ; expM <- optionMaybe (try expression);
       ; if length tys==0 && expM == Nothing then return ty
         else return (PApp (ty:tys) expM)
       }

etype :: Env -> Parser PadsTy
etype env
  =  atype env
 <|> fmap PExpression expression
 <?> "Pads etype"

atype :: Env -> Parser PadsTy
atype env
  =  try (tuple env)
 <|> do { (elm,sepM) <- brackets (listInside env)
        ; return (PList elm sepM Nothing)}
 <|> fmap PTycon qtycon
 <|> fmap PTyvar (tyvar env)

tuple :: Env -> Parser PadsTy
tuple env
  =  do { tys <- parens $ option [] (commaSep1 (ptype env))
       ; if length tys==1 then return (head tys)
         else return (PTuple tys)
       }
  <?> "Pads tuple type"



-------------------------

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
constr env =  constructor env
--         <|> constructorOp env

constructor :: Env -> Parser BranchInfo
constructor env
  = do { id  <- upperId;
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
  = do { id <- optionMaybe $ try (lowerId << reservedOp "::")
       ; ty <- ftype env
       ; predM <- optionMaybe predic
       ; return (id, ty, predM)
       }

ftype env 
  =  do { reservedOp "!"; ty <- atype env; return (IsStrict,ty)}
 <|> do { ty <- ptype env; return (NotStrict,ty)}

predic = do { reservedOp "where"; expression }



-------------------------

newRHS :: Env -> Parser BranchInfo
newRHS env
  = do { id  <- upperId;
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
    expand fty = (Nothing, fty, Nothing)

field1 :: Env -> Parser FieldInfo
field1 env
  = do { id <- lowerId; reservedOp "::"; ty <- ptype env
       ; predM <- optionMaybe predic
       ; return (Just id, (NotStrict,ty), predM)
       }

-------------------------

expression :: Parser Exp
expression =  haskellExp
          <|> literal

haskellExp :: Parser (Exp)
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
       <|> fmap (VarE . mkName) var
       <|> fmap (ConE . mkName) con
       <?> "Pads literal"

reLiteral :: Parser Exp 
reLiteral = do { reservedOp reMark
               ; str <- manyTill anyChar (reservedOp reMark) 
               ; return (ConE (mkName "RE") `AppE` LitE (StringL str))
               }
reMark = "'"

var = lowerId
con = upperId
tyvar env = try $ do { v <- var; guard (v `elem` env); return v }
qtycl = con
qtycon = tycon
tycon =  con

lowerId :: Parser String
lowerId = try (do { id <- identifier
                  ; if (isLower . head) id then return id else parserZero })

upperId :: Parser String
upperId = try (do { id <- identifier
                  ; if (isUpper . head) id then return id else parserZero })


---------------

p << q = do {x<-p;q;return x}
mymany p = option [] (many1 p)
