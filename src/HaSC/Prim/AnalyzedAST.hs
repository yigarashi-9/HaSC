module HaSC.Prim.AnalyzedAST where

import HaSC.Prim.AST
import HaSC.Prim.ObjInfo
import Text.Parsec.Pos

{- オブジェクトの情報を収集するためのデータ型 -}
type Check = Either String


{- 収集したオブジェクト情報を埋め込むための新たな木。
   A_ は Analyzed の頭文字。
   SourcePos は必要になるところだけに埋め込んでいる。 -}
type A_Program = [A_EDecl]

data A_EDecl = A_Decl ObjInfo
             | A_Func SourcePos ObjInfo [ObjInfo] A_Stmt
               deriving(Eq, Show)

data A_Stmt = A_EmptyStmt
            | A_ExprStmt     A_Expr
            | A_DeclStmt     [ObjInfo]
            | A_CompoundStmt [A_Stmt]
            | A_IfStmt       SourcePos A_Expr A_Stmt A_Stmt
            | A_WhileStmt    SourcePos A_Expr A_Stmt
            | A_ReturnStmt   SourcePos A_Expr
            | A_RetVoidStmt  SourcePos
              deriving(Eq, Show)

data A_Expr = A_AssignExpr SourcePos A_Expr A_Expr
            | A_UnaryPrim  SourcePos String A_Expr
            | A_BinaryPrim SourcePos String A_Expr A_Expr
            | A_ApplyFunc  SourcePos ObjInfo [A_Expr]
            | A_MultiExpr  [A_Expr]
            | A_Constant   Integer
            | A_IdentExpr  ObjInfo
              deriving(Eq, Show)
