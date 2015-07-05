module IntermedSyntax where

import AnalyzedAST

type IProgram = [IDecl]
type IVar  = (String, ObjInfo)

data IDecl = IVarDecl IVar
           | IFunDecl IVar [IVar] IStmt
             deriving(Show, Eq, Ord)

data IStmt = IEmpty
           | ILet      IVar IExpr
           | IWrite    IVar IVar
           | IRead     IVar IVar
           | IIf       IVar [IStmt] [IStmt]
           | IWhile    IVar [IStmt]
           | ICall     IVar IVar [IVar]
           | IReturn   IVar
           | IPrint    IVar
           | ICompound [IDecl] [IStmt]
             deriving(Show, Eq, Ord)

data IExpr = IVarExp IVar
           | IInt    Integer
           | IAop    String IVar IVar
           | IRelop  String IVar IVar
           | IAddr   IVar
             deriving(Show, Eq, Ord)
