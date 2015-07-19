module HaSC.Prim.IntermedSyntax where

import HaSC.Prim.ObjInfo

type IProgram = [IDecl]
type IVar     = ObjInfo
type Label = String

data IDecl = IVarDecl IVar
           | IFunDecl IVar [IVar] [ICode]
             deriving(Show, Eq, Ord)

data ICode = ILabel     Label
           | ILet       IVar IVar
           | ILi        IVar Integer
           | IAop       String IVar IVar IVar
           | IRelop     String IVar IVar IVar
           | IWrite     IVar IVar
           | IRead      IVar IVar
           | IAddr      IVar IVar
           | IJumpTr    IVar Label
           | IJumpFls   IVar Label
           | IJump      Label
           | ICall      IVar IVar [IVar]
           | IReturn    IVar
           | IRetVoid
           | IPrint     IVar
             deriving(Show, Eq, Ord)
