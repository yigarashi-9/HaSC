module AnalyzedAST where

import Text.Parsec.Pos
import Data.List

data ObjInfo  = ObjInfo { kind :: Kind, ctype :: CType, level :: Level}
                deriving(Eq, Show, Ord)

data Kind  = Var | Func | FuncProto | Parm deriving(Show, Eq, Ord)

data CType = CInt
           | CVoid
           | CNone
           | CPointer CType
           | CArray   CType Integer
           | CFun     CType [CType]
           deriving(Eq, Ord)

instance Show CType where
    show (CInt)           = "int"
    show (CVoid)          = "void"
    show (CNone)          = ""
    show (CPointer CInt)  = "int *"
    show (CArray ty size) = show ty ++ "[" ++ show size ++ "]"
    show (CFun ty args)   = concat ["(", concat $ intersperse ", " (map show args),
                                    ") -> ", show ty]
    show _                = "*** complex type... cannot show ***"

type Level = Int


type A_Program = [A_EDecl]
type A_Idnentifier = (String, ObjInfo)

data A_EDecl = A_Decl A_Idnentifier
             | A_Func SourcePos A_Idnentifier [A_Idnentifier] A_Stmt
               deriving(Eq, Show)

data A_Stmt = A_EmptyStmt
            | A_ExprStmt     A_Expr
            | A_DeclStmt     [A_Idnentifier]
            | A_CompoundStmt [A_Stmt]
            | A_IfStmt       SourcePos A_Expr A_Stmt A_Stmt
            | A_WhileStmt    SourcePos A_Expr A_Stmt
            | A_ReturnStmt   SourcePos A_Expr
            | A_RetVoidStmt  SourcePos
              deriving(Eq, Show)

data A_Expr = A_AssignExpr SourcePos A_Expr A_Expr
            | A_UnaryPrim  SourcePos String A_Expr
            | A_BinaryPrim SourcePos String A_Expr A_Expr
            | A_ApplyFunc  SourcePos A_Idnentifier [A_Expr]
            | A_MultiExpr  [A_Expr]
            | A_Constant   Integer
            | A_IdentExpr  A_Idnentifier
              deriving(Eq, Show)
