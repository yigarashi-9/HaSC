{-# LANGUAGE RankNTypes #-}
module Parser where

import           Text.Parsec
import           Text.Parsec.String
import           Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import           Text.Parsec.Language

import Control.Monad.Identity(Identity)
import Control.Monad(liftM, liftM2, liftM3)
import Control.Applicative((<*))

import AST

parseProgram :: String -> IO Program
parseProgram fileName = parseFromFile smallC fileName >>= either report return
  where report err = error $ show err



{- **************************
             lexer
   ************************** -}

def :: LanguageDef st
def = javaStyle {
        P.reservedNames   = ["if", "else", "while", "for", "return", "int", "void"],
        P.reservedOpNames = ["=", "==", "!=", "&", "&&", "|", "||", "+", "-", "*", "/",
                             "<", ">", "<=", ">="] }

lexer :: P.TokenParser st
lexer = P.makeTokenParser def

operator :: Parser String
operator = P.operator lexer

identifier :: Parser String
identifier = P.identifier lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

parens :: forall a. Parser a -> Parser a
parens = P.parens lexer

brackets :: forall a. Parser a -> Parser a
brackets = P.brackets lexer

braces :: forall a. Parser a -> Parser a
braces = P.braces lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

symbol :: String -> Parser String
symbol = P.symbol lexer

natural :: Parser Integer
natural = P.natural lexer

semi :: Parser String
semi = P.semi lexer

comma :: Parser String
comma = P.comma lexer



{- *************************
            Parser
   ************************* -}

smallC :: Parser Program
smallC = whiteSpace >> many1 externalDecl

externalDecl :: Parser EDecl
externalDecl =  try (liftM2 Decl getPosition decl)
            <|> try funcPrototype
            <|> funcDef
            <?> "External Declaration"

decl :: Parser DeclList
decl = do
  ty    <- typeSpecifier
  decls <- declarator `sepBy1` (symbol ",") <* semi
  return (genDecl ty decls)

genDecl :: DeclType -> [(String, DirectDecl)] -> DeclList
genDecl ty = foldr f []
    where f (p, dec) acc = (checkPointer p ty, dec):acc

declarator :: Parser (String, DirectDecl)
declarator = do
  p   <- pointerOp
  dec <- directDecl
  return (p, dec)

directDecl :: Parser DirectDecl
directDecl = try ( do
                   pos  <- getPosition
                   name <- identifier
                   size <- brackets natural
                   return $ Sequence pos name size )
          <|> (do
                pos  <- getPosition
                name <- identifier
                return $ Variable pos name)

funcPrototype :: Parser EDecl
funcPrototype = do
  pos <- getPosition
  ty  <- typeSpecifier
  (p, name, parms) <- funcDeclarator <* semi
  return $ FuncPrototype pos (checkPointer p ty) name parms

funcDeclarator :: Parser (String, Identifier, [(DeclType, Identifier)])
funcDeclarator = do
  p     <- pointerOp
  name  <- identifier
  parms <- parens $ parameterDecl `sepBy` (symbol ",")
  return $ (p, name, parms)

parameterDecl :: Parser (DeclType, Identifier)
parameterDecl = do
  ty   <- typeSpecifier
  p    <- pointerOp
  name <- identifier
  return (checkPointer p ty, name)

typeSpecifier :: Parser DeclType
typeSpecifier =  (symbol "int"  >> return DeclInt)
             <|> (symbol "void" >> return DeclVoid)

funcDef :: Parser EDecl
funcDef = do
  pos <- getPosition
  ty  <- typeSpecifier
  (p, name, parms) <- funcDeclarator
  stmts <- liftM2 CompoundStmt getPosition compoundStmt
  return $ FuncDef pos (checkPointer p ty) name parms stmts

stmt :: Parser Stmt
stmt =  try (semi >> liftM EmptyStmt getPosition)
    <|> try (liftM2 ExprStmt getPosition expr <* semi)
    <|> try (liftM2 CompoundStmt getPosition compoundStmt)
    <|> try ifElseStmt
    <|> try ifStmt
    <|> try whileStmt
    <|> try forStmt
    <|> returnStmt <* semi
    <?> "Statement"

compoundStmt :: Parser [Stmt]
compoundStmt = braces
               (do
                 decls <- many $ liftM2 (,) getPosition (try decl)
                 stmts <- liftM flattenCompoundStmt (many $ try stmt)
                 return $ (map (uncurry DeclStmt) decls) ++ stmts)

flattenCompoundStmt :: [Stmt] -> [Stmt]
flattenCompoundStmt = foldr flatten []
    where flatten s acc =
              case s of
                (CompoundStmt _ stmts) -> stmts ++ acc
                stmt                   -> stmt:acc


ifStmt :: Parser Stmt
ifStmt = do
  pos  <- getPosition
  cond <- symbol "if" >> parens expr
  true <- stmt
  return $ IfStmt pos cond true (EmptyStmt pos)

ifElseStmt :: Parser Stmt
ifElseStmt = do
  pos  <- getPosition
  cond <- symbol "if"   >> parens expr
  true <- stmt
  fals <- symbol "else" >> stmt
  return $ IfStmt pos cond true fals

whileStmt :: Parser Stmt
whileStmt = do
  pos  <- getPosition
  cond <- symbol "while" >> parens expr
  body <- stmt
  return $ WhileStmt pos cond body

forStmt :: Parser Stmt
forStmt = do
  pos    <- getPosition <* (symbol "for" >> symbol "(")
  init   <- option (EmptyStmt pos) (liftM (ExprStmt pos) expr) <* semi
  cond   <- option (Constant pos 1) expr <* semi
  update <- option (EmptyStmt pos) (liftM (ExprStmt pos) expr) <* symbol ")"
  body   <- stmt
  return $ forToWhile pos body init cond update

returnStmt :: Parser Stmt
returnStmt = do
  pos <- getPosition
  symbol "return"
  (liftM (ReturnStmt pos) (try expr)) <|> (return $ RetVoidStmt pos)

expr :: Parser Expr
expr = do
  p <- getPosition
  e <- (assignExpr `sepBy` comma)
  case e of
    [e'] -> return e'
    []   -> fail "bad expr"
    _    -> return $ MultiExpr p e

assignExpr :: Parser Expr
assignExpr = try assign <|> logicalOrExpr <?> "Assign Expr"

assign :: Parser Expr
assign = do
  dest <- logicalOrExpr
  pos  <- getPosition
  src  <- reservedOp "=" >> assignExpr
  return $ AssignExpr pos dest src

logicalOrExpr :: Parser Expr
logicalOrExpr = liftM compressPointer $ buildExpressionParser table postFixExpr

postFixExpr :: Parser Expr
postFixExpr =  try (postFixHeader >>= arrayAccess)
           <|> postFixHeader
           <?> "Postfix Expression"

postFixHeader :: Parser Expr
postFixHeader =  try (liftM3 ApplyFunc getPosition
                      identifier (parens $ assignExpr `sepBy` comma))
             <|> primaryExpr
             <?> "Postfix Header"

arrayAccess :: Expr -> Parser Expr
arrayAccess h = do
  accesser <- many1 $ liftM2 (,) getPosition (brackets expr)
  return $ foldl (\acc (p, i) -> UnaryPrim p "*" (BinaryPrim p "+" acc i)) h accesser

primaryExpr :: Parser Expr
primaryExpr =  try (parens expr)
           <|> liftM2 Constant getPosition natural
           <|> liftM2 IdentExpr getPosition identifier
           <?> "Primary Expr"



{- **************************
            Utility
   ************************** -}

checkPointer :: String -> DeclType -> DeclType
checkPointer p ty = if p == "*" then DeclPointer ty else ty

pointerOp :: Parser String
pointerOp = option "" (symbol "*")

table :: [[Operator String () Identity Expr]]
table = [(Prefix op_neg):(map op_prefix ["&", "*"]),
         map op_infix ["*", "/"],
         map op_infix ["+", "-"],
         map op_infix ["<=", "<", ">", ">="],
         map op_infix ["==", "!="],
         map op_infix ["&&"],
         map op_infix ["||"]]
    where
      op_func f s = do {
                      pos <- getPosition <* (reservedOp s);
                      return $ f pos s;   }
      op_neg      = do {
                      pos <- getPosition <* (reservedOp "-");
                      return $ BinaryPrim pos "*" (Constant pos (-1)) }
      op_prefix s = Prefix (op_func UnaryPrim s)
      op_infix  s = Infix  (op_func BinaryPrim s) AssocLeft

forToWhile :: SourcePos -> Stmt -> Stmt -> Expr -> Stmt -> Stmt
forToWhile pos body init cond update
    = CompoundStmt pos [init, WhileStmt pos cond whileBody]
    where
      whileBody = case body of
                    (CompoundStmt p stmts) -> (CompoundStmt p
                                               (stmts ++ [update]))
                    stmt -> (CompoundStmt pos [stmt, update])

compressPointer :: Expr -> Expr
compressPointer (UnaryPrim _ "*" (UnaryPrim _ "&" e)) = compressPointer e
compressPointer (UnaryPrim _ "&" (UnaryPrim _ "*" e)) = compressPointer e
compressPointer (UnaryPrim p op e)      = (UnaryPrim p op $ compressPointer e)
compressPointer (BinaryPrim p op e1 e2) = (BinaryPrim p op (compressPointer e1)
                                                           (compressPointer e2))
compressPointer (ApplyFunc p f es)      = (ApplyFunc p f (map compressPointer es))
compressPointer (AssignExpr p e1 e2)    = (AssignExpr p (compressPointer e1)
                                                        (compressPointer e2))
compressPointer (MultiExpr p es)        = (MultiExpr p (map compressPointer es))
compressPointer e = e
