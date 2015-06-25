module AST where

import Text.Parsec

type Program    = [EDecl]
type Identifier = String
type DeclList   = [(DeclType, DirectDecl)] -- Variable Declaration List - int a, *b ...


data EDecl = Decl          SourcePos DeclList
           | FuncPrototype SourcePos DeclType Identifier [(DeclType, Identifier)]
           | FuncDef       SourcePos DeclType Identifier [(DeclType, Identifier)] Stmt
           deriving(Show)

instance Eq EDecl where
    (==) (Decl _ dl) (Decl _ dl')
        = (dl == dl')
    (==) (FuncPrototype _ ty n a) (FuncPrototype _ ty' n' a')
        = (ty == ty') && (n == n') && (a == a')
    (==) (FuncDef _ ty n a s) (FuncDef _ ty' n' a' s')
        = (ty == ty') && (n == n') && (a == a') && (s == s')
    (==) _ _ = False

data DeclType = DeclPointer DeclType
              | DeclInt
              | DeclVoid
              deriving(Eq, Show)

data DirectDecl = Variable SourcePos Identifier
                | Sequence SourcePos Identifier Integer
                deriving(Show)

instance Eq DirectDecl where
    (==) (Variable _ i)    (Variable _ i')     = i == i'
    (==) (Sequence _ i si) (Sequence _ i' si') = i == i' && si == si'
    (==) _ _ = False


data Stmt = EmptyStmt    SourcePos
          | ExprStmt     SourcePos Expr
          | DeclStmt     SourcePos DeclList
          | CompoundStmt SourcePos [Stmt]
          | IfStmt       SourcePos Expr Stmt Stmt
          | WhileStmt    SourcePos Expr Stmt
          | ReturnStmt   SourcePos Expr
          | RetVoidStmt  SourcePos
          deriving(Show)

instance Eq Stmt where
    (==) (EmptyStmt _)       (EmptyStmt _)         = True
    (==) (ExprStmt _ e)      (ExprStmt _ e')       = e == e'
    (==) (DeclStmt _ d)      (DeclStmt _ d')       = d == d'
    (==) (CompoundStmt _ ls) (CompoundStmt _ ls')  = ls == ls'
    (==) (IfStmt _ e tr fl)  (IfStmt _ e' tr' fl') = e == e' && tr == tr' && fl == fl'
    (==) (WhileStmt _ e stm) (WhileStmt _ e' stm') = e == e' && stm == stm'
    (==) (ReturnStmt _ e)    (ReturnStmt _ e')     = e == e'
    (==) (RetVoidStmt _)     (RetVoidStmt _)       = True
    (==) _                   _                     = False


data Expr = AssignExpr   SourcePos Expr Expr
          | UnaryPrim    SourcePos String Expr
          | BinaryPrim   SourcePos String Expr Expr
          | ApplyFunc    SourcePos Identifier [Expr]
          | MultiExpr    SourcePos [Expr]
          | Constant     SourcePos Integer
          | IdentExpr    SourcePos Identifier
          deriving(Show)

instance Eq Expr where
    (==) (AssignExpr _ d s)      (AssignExpr _ d' s')  = d == d' && s == s'
    (==) (UnaryPrim _ op e)      (UnaryPrim _ op' e')  = op == op' && e == e'
    (==) (ApplyFunc _ d s)       (ApplyFunc _ d' s')   = d == d' && s == s'
    (==) (MultiExpr _ es)        (MultiExpr _ es')     = es == es'
    (==) (MultiExpr _ [e])       e'                    = e == e'
    (==) e'                      (MultiExpr _ [e])     = e == e'
    (==) (Constant _ c)          (Constant _ c')       = c == c'
    (==) (IdentExpr _ i)         (IdentExpr _ i')      = i == i'
    (==) (BinaryPrim _ op e1 e2) (BinaryPrim _ op' e1' e2')
        = op == op' && e1 == e1' && e2 == e2'
    (==) _ _ = False
