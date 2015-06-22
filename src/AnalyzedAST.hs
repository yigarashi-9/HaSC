module AnalyzedAST where

import Text.Parsec.Pos
import Environment

type A_Program    = [A_EDecl]

data A_EDecl = A_Decl Info
             | A_Func SourcePos Info [Info] A_Stmt
               deriving(Eq, Show)

data A_Stmt = A_EmptyStmt
            | A_ExprStmt     A_Expr
            | A_DeclStmt     [Info]
            | A_CompoundStmt [A_Stmt]
            | A_IfStmt       SourcePos A_Expr A_Stmt A_Stmt
            | A_WhileStmt    SourcePos A_Expr A_Stmt
            | A_ReturnStmt   SourcePos A_Expr
            | A_RetVoidStmt  SourcePos
              deriving(Eq, Show)

data A_Expr = A_AssignExpr SourcePos A_Expr A_Expr
            | A_UnaryPrim  SourcePos String A_Expr
            | A_BinaryPrim SourcePos String A_Expr A_Expr
            | A_ApplyFunc  SourcePos Info [A_Expr]
            | A_MultiExpr  [A_Expr]
            | A_Constant   Integer
            | A_IdentExpr  Info
              deriving(Eq, Show)
