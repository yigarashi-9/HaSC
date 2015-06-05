module AST where

import Text.Parsec

type Program    = [EDecl]
type Identifier = String
type DeclList   = [(Type, DirectDecl)] -- Variable Declaration List - int a, *b ...


data EDecl = Decl          SourcePos DeclList
           | FuncPrototype SourcePos Type Identifier [(Type, Identifier)]
           | FuncDef       SourcePos Type Identifier [(Type, Identifier)] Stmt
           deriving(Show)

instance Eq EDecl where
    (Decl _ dl)              == (Decl _ dl') = dl == dl'
    (FuncPrototype _ ty n a) == (FuncPrototype _ ty' n' a')
        = ty == ty' && n == n' && a == a'
    (FuncDef _ ty n a s)     == (FuncDef _ ty' n' a' s')
        = ty == ty' && n == n' && a == a' && s == s'
    _ == _ = False

data Type = CPointer Type
          | CInt
          | CVoid
          deriving(Eq)

instance Show Type where
    show (CPointer CInt) = "int *"
    show (CInt)          = "int "
    show (CVoid)         = "void "
    show _               = error "invalid type"

data DirectDecl = Variable SourcePos Identifier
                | Sequence SourcePos Identifier Integer

instance Show DirectDecl where
    show (Variable _ s) = show s
    show (Sequence _ s size) = concat [show s, "[", show size, "]"]

instance Eq DirectDecl where
    (Variable _ i)    == (Variable _ i')     = i == i'
    (Sequence _ i si) == (Sequence _ i' si') = i == i' && si == si'
    _                 == _                   = False


data Stmt = EmptyStmt    SourcePos
          | ExprStmt     SourcePos Expr
          | DeclStmt     SourcePos DeclList
          | CompoundStmt SourcePos [Stmt]
          | IfStmt       SourcePos Expr Stmt Stmt
          | WhileStmt    SourcePos Expr Stmt
          | ReturnStmt   SourcePos Expr
          deriving(Show)

instance Eq Stmt where
    (EmptyStmt _)       == (EmptyStmt _)         = True
    (ExprStmt _ e)      == (ExprStmt _ e')       = e == e'
    (DeclStmt _ d)      == (DeclStmt _ d')       = d == d'
    (CompoundStmt _ ls) == (CompoundStmt _ ls')  = ls == ls'
    (IfStmt _ e tr fl)  == (IfStmt _ e' tr' fl') = e == e' && tr == tr' && fl == fl'
    (WhileStmt _ e stm) == (WhileStmt _ e' stm') = e == e' && stm == stm'
    (ReturnStmt _ e)    == (ReturnStmt _ e')     = e == e'
    _                   == _                     = False


data Expr = AssignExpr   SourcePos Expr Expr
          | UnaryPrim    SourcePos String Expr
          | BinaryPrim   SourcePos String Expr Expr
          | ArrayAccess  SourcePos Expr Expr
          | ApplyFunc    SourcePos String [Expr]
          | MultiExpr    SourcePos [Expr]
          | Constant     SourcePos Integer
          | IdentExpr    SourcePos Identifier
          deriving(Show)

instance Eq Expr where
    (AssignExpr _ d s)      == (AssignExpr _ d' s')  = d == d' && s == s'
    (UnaryPrim _ op e)      == (UnaryPrim _ op' e')  = op == op' && e == e'
    (BinaryPrim _ op e1 e2) == (BinaryPrim _ op' e1' e2')
        = op == op' && e1 == e1' && e2 == e2'
    (ArrayAccess _ d s)     == (ArrayAccess _ d' s') = d == d' && s == s'
    (ApplyFunc _ d s)       == (ApplyFunc _ d' s')   = d == d' && s == s'
    (MultiExpr _ es)        == (MultiExpr _ es')     = es == es'
    (MultiExpr _ [e])       == e'                    = e == e'
    e'                      == (MultiExpr _ [e])     = e == e'
    (Constant _ c)          == (Constant _ c')       = c == c'
    (IdentExpr _ i)         == (IdentExpr _ i')      = i == i'
    _                       == _                     = False
