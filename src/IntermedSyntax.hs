module IntermedSyntax where

import AnalyzedAST

type IProgram = [IDecl]


data IVar  = Info { info :: RawInfo }
           | Addr { addr :: Address }
             deriving(Eq, Ord)

instance Show IVar where
    show (Info i) = show i
    show (Addr a) = show a

ivarName :: IVar -> String
ivarName = fst . info

ivarType :: IVar -> CType
ivarType = ctype . snd . info

getFpAddr :: IDecl -> Int
getFpAddr (IVarDecl ivar) = case addr ivar of
                              (Fp n)       -> n
                              (Gp n)       -> 0
                              (Register n) -> 0

type RawInfo = (String, ObjInfo)
data Address = Fp Int | Gp Int | Register Int deriving(Eq, Ord)


instance Show Address where
    show (Fp n) = show n ++ "($fp)"
    show (Gp n) = show n ++ "($gp)"
    show (Register n) = show n -- どうにかして

data IDecl = IVarDecl IVar
           | IFunDecl IVar [IVar] IStmt
             deriving(Show, Eq, Ord)

data IStmt = IEmpty
           | ILet      IVar IExpr
           | IWrite    IVar IVar
           | IRead     IVar IVar
           | IIf       IVar [IStmt] [IStmt]
           | IWhile    IVar [IStmt]
           | ICall     IVar IVar [IVar]  -- ICall dest func [arg]
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
