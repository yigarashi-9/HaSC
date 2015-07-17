module HaSC.Prim.ObjInfo where

import Data.List


data ObjInfo  = ObjInfo { objName  :: String,
                          objKind  :: Kind,
                          objCtype :: CType,
                          objLevel :: Level
                        }deriving(Eq, Ord)

instance Show ObjInfo where
    show (ObjInfo name _ _ lev) = name ++ ":" ++ show lev

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
    (==) CTemp            CTemp            = True
    (==) CVoid            CVoid            = True
    (==) (CPointer ty1)   (CPointer ty2)   = ty1 == ty2
    (==) (CArray ty1 _)   (CArray ty2 _)   = ty1 == ty2
    (==) (CFun ty1 args1) (CFun ty2 args2) = (ty1 == ty2) && (args1 == args2)
    (==) (CArray ty1 _)   (CPointer ty2)   = ty1 == ty2
    (==) (CPointer ty1)   (CArray ty2 _)   = ty1 == ty2
    (==) _ _ = False

containVoid :: CType -> Bool
containVoid (CVoid)       = True
containVoid (CArray ty _) = containVoid ty
containVoid (CPointer ty) = containVoid ty
containVoid _             = False

-- void かそうでないかが重要なので、max を使って型を合成する
synType :: CType -> CType -> CType
synType = max
