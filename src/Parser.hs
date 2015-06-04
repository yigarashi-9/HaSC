{-# LANGUAGE RankNTypes #-}
module Parser where

import           Text.Parsec
import           Text.Parsec.String
import           Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import           Text.Parsec.Language

import Control.Monad.Identity(Identity)
import Control.Monad(liftM, liftM2, liftM3)

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
smallC = many1 externalDecl

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

genDecl :: Type -> [(String, DirectDecl)] -> DeclList
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

funcDeclarator :: Parser (String, Identifier, [(Type, Identifier)])
funcDeclarator = do
  p     <- pointerOp
  name  <- identifier
  parms <- parens $ parameterDecl `sepBy` (symbol ",")
  return $ (p, name, parms)

parameterDecl :: Parser (Type, Identifier)
parameterDecl = do
  ty   <- typeSpecifier
  p    <- pointerOp
  name <- identifier
  return (checkPointer p ty, name)

typeSpecifier :: Parser Type
typeSpecifier =  (symbol "int"  >> return CInt)
             <|> (symbol "void" >> return CVoid)

funcDef :: Parser EDecl
funcDef = do
  pos <- getPosition
  ty  <- typeSpecifier
  (p, name, parms) <- funcDeclarator
  stmts <- compoundStmt
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
                 decls <- many $ liftTM getPosition (try decl)
                 stmts <- many (try stmt)
                 return $ (map (uncurry DeclStmt) decls) ++ stmts)

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
  dec    <- expr <* semi
  cond   <- expr <* semi
  update <- expr <* symbol ")"
  body   <- stmt
  return $ CompoundStmt pos [ExprStmt pos dec,
                             WhileStmt pos cond
                                           (CompoundStmt pos [body, ExprStmt pos update])]

returnStmt :: Parser Stmt
returnStmt = do
  pos <- getPosition
  symbol "return" >> liftM (ReturnStmt pos) expr

expr :: Parser Expr
expr = liftM2 MultiExpr getPosition (assignExpr `sepBy` comma)

assignExpr :: Parser Expr
assignExpr = try assign <|> logicalOrExpr <?> "Assign Expr"

assign :: Parser Expr
assign = do
  dest <- logicalOrExpr
  pos  <- getPosition
  src  <- reservedOp "=" >> assignExpr
  return $ AssignExpr pos dest src

logicalOrExpr :: Parser Expr
logicalOrExpr = buildExpressionParser table postFixExpr

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
  accesser <- many1 $ liftTM getPosition (brackets expr)
  return $ foldl (\acc (p, i) -> ArrayAccess p acc i) h accesser

primaryExpr :: Parser Expr
primaryExpr =  try (parens expr)
           <|> liftM2 Constant getPosition natural
           <|> liftM2 IdentExpr getPosition identifier
           <?> "Primary Expr"



{- **************************
            Utility
   ************************** -}

checkPointer :: String -> Type -> Type
checkPointer p ty = if p == "*" then CPointer ty else ty

pointerOp :: Parser String
pointerOp = option "" (symbol "*")

table :: [[Operator String () Identity Expr]]
table = [map op_prefix ["-", "&", "*"],
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
      op_prefix s = Prefix (op_func UnaryPrim s)
      op_infix  s = Infix  (op_func BinaryPrim s) AssocLeft

liftTM :: Monad m => m a -> m b -> m (a, b)
liftTM ma mb = do
  a <- ma
  b <- mb
  return (a, b)
