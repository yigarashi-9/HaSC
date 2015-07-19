module HaSC.MIPS.IntermedSyntax where

type MProgram = [MDecl]
type Label = String

data MVar  = Fp Int
           | Gp Int
             deriving(Eq, Ord)

instance Show MVar where
    show (Fp n)    = show n ++ "($fp)"
    show (Gp n)    = show n ++ "($gp)"

data MDecl = MVarDecl MVar
           | MFunDecl Int String [MVar] [MCode]
             deriving(Show, Eq, Ord)

data MCode = MLabel     Label
           | MLi        MVar Integer
           | MLet       MVar MVar
           | MAop       String MVar MVar MVar
           | MRelop     String MVar MVar MVar
           | MWrite     MVar MVar
           | MRead      MVar MVar
           | MAddr      MVar MVar
           | MJumpTr    MVar Label
           | MJumpFls   MVar Label
           | MJump      Label
           | MCall      MVar String [MVar]
           | MReturn    MVar
           | MRetVoid
           | MPrint     MVar
             deriving(Show, Eq, Ord)
