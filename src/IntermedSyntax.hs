module IntermedSyntax where

import AnalyzedAST

type IVar  = (String, ObjInfo)

data IDecl = IVarDecl IVar
           | IFunDecl IVar [IVar] IStmt


data IStmt = IEmpty
           | ILet IVar IExp
           | IWrite IVar IVar
           |
