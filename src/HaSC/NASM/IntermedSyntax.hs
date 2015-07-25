module HaSC.NASM.IntermedSyntax where

type NProgram = [NDecl]
type Label = String

data NVar  = Bp Int
           | Gb String CType
             deriving(Eq, Ord)

instance Show NVar where
    show (Bp n)    = "[ebp + " ++ show n ++ "]"
    show (Gb l _)  = l

data NDecl = NVarDecl NVar
           | NFunDecl Int String [NCode]
             deriving(Show, Eq, Ord)

data NCode = NLabel     Label
           | NLi        NVar Integer
           | NLet       NVar NVar
           | NAop       String NVar NVar NVar
           | NRelop     String NVar NVar NVar
           | NWrite     NVar NVar
           | NRead      NVar NVar
           | NAddr      NVar NVar
           | NJumpTr    NVar Label
           | NJumpFls   NVar Label
           | NJump      Label
           | NCall      NVar String [NVar]
           | NReturn    NVar
           | NRetVoid
           | NPrint     NVar
             deriving(Show, Eq, Ord)
