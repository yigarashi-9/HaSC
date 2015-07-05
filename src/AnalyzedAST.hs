module AnalyzedAST where

import AST
import Text.Parsec.Pos
import Data.List
import Control.Monad.State.Strict

{- オブジェクトの情報を収集するためのデータ型 -}

type Check = Either String

data ObjInfo  = ObjInfo { kind  :: Kind,
                          ctype :: CType,
                          level :: Level
                        }deriving(Eq, Ord)

instance Show ObjInfo where
    show (ObjInfo _ _ lev) = show lev

type Level = Int
data Kind  = Var | Func | FuncProto | Parm deriving(Show, Eq, Ord)
data CType = CVoid
           | CInt
           | CPointer CType
           | CArray   CType Integer
           | CFun     CType [CType]
           | CTemp
           deriving(Ord)

instance Show CType where
    show (CInt)           = "int"
    show (CTemp)          = "temp"
    show (CVoid)          = "void"
    show (CPointer CInt)  = "int *"
    show (CArray ty size) = show ty ++ "[" ++ show size ++ "]"
    show (CFun ty args)   = concat ["(", concat $ intersperse ", " (map show args),
                                    ") -> ", show ty]
    show _                = "*** complex type... cannot show ***"

instance Eq CType where
    (==) CInt             CInt             = True
    (==) CVoid            CVoid            = True
    (==) (CPointer ty1)   (CPointer ty2)   = ty1 == ty2
    (==) (CArray ty1 _)   (CArray ty2 _)   = ty1 == ty2
    (==) (CFun ty1 args1) (CFun ty2 args2) = (ty1 == ty2) && (args1 == args2)
    (==) (CArray ty1 _)   (CPointer ty2)   = ty1 == ty2
    (==) (CPointer ty1)   (CArray ty2 _)   = ty1 == ty2
    (==) _ _ = False


convType :: DeclType -> CType
convType (DeclPointer ty) = CPointer (convType ty)
convType (DeclInt)        = CInt
convType (DeclVoid)       = CVoid

containVoid :: CType -> Bool
containVoid (CVoid)       = True
containVoid (CArray ty _) = containVoid ty
containVoid (CPointer ty) = containVoid ty
containVoid _             = False

-- void かそうでないかが重要なので、max を使って型を合成する
synType :: CType -> CType -> CType
synType = max

{- 収集したオブジェクト情報を埋め込むための新たな木。
   A_ は Analyzed の頭文字。
   SourcePos は必要になるところだけに埋め込んでいる。 -}

type A_Program = [A_EDecl]
type A_Idnentifier = (Identifier, ObjInfo)

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
