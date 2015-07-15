module IntermedSyntax where

import ObjInfo

type IProgram = [IDecl]

data IVar  = Info  { info  :: ObjInfo }
           | Const { const :: Integer }
             deriving(Eq, Ord)

instance Show IVar where
    show (Info i)  = show i
    show (Const n) = show n

ivarName :: IVar -> String
ivarName = objName . info

ivarType :: IVar -> CType
ivarType = objCtype . info

data IDecl = IVarDecl IVar
           | IFunDecl IVar [IVar] [ICode]
             deriving(Show, Eq, Ord)

type Label = String

data ICode = ILabel     Label
           | ILet       IVar IVar
           | IAop       String IVar IVar IVar
           | IRelop     String IVar IVar IVar
           | IWrite     IVar IVar
           | IRead      IVar IVar
           | IAddr      IVar IVar
           | IJumpTr    IVar Label
           | IJumpFls   IVar Label
           | IJump      Label
           | ICall      IVar IVar [IVar]  -- ICall dest func [arg]
           | IReturn    IVar
           | IPrint     IVar
             deriving(Show, Eq, Ord)
