module MIPS.GenCode where

import Data.Maybe
import Data.List
import Control.Monad.State
import Debug.Trace

import AnalyzedAST
import ObjInfo
import IntermedSyntax
import MIPS.IntermedSyntax

type MIPSCode  = String
type MIPSTerms = [[String]]


mipsGenCode :: MProgram -> MIPSCode
mipsGenCode prog = header ++ (insertNL . map insertWS
                             $ runLabelEnv (liftM concat $ mapM genMDecl prog))
    where header = "    .text\n    .globl main\n"


type LabelEnv = State Int

runLabelEnv :: LabelEnv a -> a
runLabelEnv s = evalState s 0

setFrameSize :: Int -> LabelEnv ()
setFrameSize = put

getCurFrameSize :: LabelEnv Int
getCurFrameSize = get

genMDecl :: MDecl -> LabelEnv MIPSTerms
genMDecl (MFunDecl frameSize f args body)
    = do let fpSize = (length args) * wordSize
             size   = - frameSize + fpSize + wordSize * 2
         setFrameSize size
         stmt <- liftM concat $ mapM genStmt body
         return $ [[f ++ ":"]]
                  ++ [["move", (t 0), "$sp"],
                      ["addi", "$sp", "$sp", show (-size)],
                      ["sw", "$fp", "0($sp)"],
                      ["sw", "$ra", "4($sp)"],
                      ["addi", "$fp", (t 0), show (-fpSize)]]
                  ++ stmt
                  ++ [["lw", "$ra", "4($sp)"],
                      ["lw", "$fp", "0($sp)"],
                      ["addi", "$sp", "$sp", show size],
                      ["jr", "$ra"]]
genMDecl _ = return [[""]]

genStmt :: MCode -> LabelEnv MIPSTerms
genStmt (MLabel label)      = return [[label ++ ":"]]
genStmt (MLi dest n)        = return [["li", (t 0), show n],
                                      ["sw", (t 0), show dest]]
genStmt (MLet dest src)     = return [["lw", (t 0), show src],
                                      ["sw", (t 0), show dest]]
genStmt (MAop op d e1 e2)   = return $ [["lw", (t 1), show e1],
                                        ["lw", (t 2), show e2]]
                                       ++ mipsArithOp op
                                       ++ [["sw", (t 0), show d]]
genStmt (MRelop op d e1 e2) = return $ [["lw", (t 1), show e1],
                                        ["lw", (t 2), show e2]]
                                       ++ mipsRelOp op
                                       ++ [["sw", (t 0), show d]]
genStmt (MWrite dest src)   = return [["lw", (t 1), show src],
                                      ["lw", (t 0), show dest],
                                      ["sw", (t 1), "0(" ++ (t 0) ++ ")"]]
genStmt (MRead dest src)    = return [["lw", (t 1), show src],
                                      ["lw", (t 0), "0(" ++ (t 1) ++ ")"],
                                      ["sw", (t 0), show dest]]
genStmt (MAddr dest src)    = return $ (case src of
                                          Fp n -> [["addi", (t 0), "$fp", show n]]
                                          Gp n -> [["addi", (t 0), "$gp", show n]]
                                          _    -> undefined)
                                       ++ [["sw", (t 0), show dest]]
genStmt (MJumpTr cond lab)  = return [["lw", (t 0), show cond],
                                      ["bne", (t 0), zero, lab]]
genStmt (MJumpFls cond lab) = return [["lw", (t 0), show cond],
                                      ["beq", (t 0), zero, lab]]
genStmt (MJump lab)         = return [["j", lab]]
genStmt (MCall dest f args) = return $ setArgs args
                                       ++ [["jal", f],
                                           ["sw", (v 0), show dest]]
genStmt (MReturn ivar)      = do frameSize <- getCurFrameSize
                                 return $ [["lw", (v 0), show ivar],
                                           ["lw", "$ra", "4($sp)"],
                                           ["lw", "$fp", "0($sp)"],
                                           ["addi", "$sp", "$sp", show frameSize],
                                           ["jr", "$ra"]]
genStmt (MRetVoid)          = do frameSize <- getCurFrameSize
                                 return $ [["lw", "$ra", "4($sp)"],
                                           ["lw", "$fp", "0($sp)"],
                                           ["addi", "$sp", "$sp", show frameSize],
                                           ["jr", "$ra"]]
genStmt (MPrint ivar)       = return [["li", (v 0), show 1],
                                      ["lw", (a 0), show ivar],
                                      ["syscall"]]

setArgs :: [MVar] -> MIPSTerms
setArgs = fst . foldr foldArgs ([], -4)
    where
      foldArgs :: MVar -> (MIPSTerms, Int) -> (MIPSTerms, Int)
      foldArgs arg (acc, addr) = ([["lw", (t 0), show arg],
                                   ["sw", (t 0), show addr ++ "($sp)"]]
                                  ++ acc,
                                  addr - wordSize)

mipsArithOp :: String -> MIPSTerms
mipsArithOp "+" = [["add", (t 0), (t 1), (t 2)]]
mipsArithOp "-" = [["sub", (t 0), (t 1), (t 2)]]
mipsArithOp "*" = [["mult", (t 1), (t 2)],
                   ["mflo", (t 0)]]
mipsArithOp "/" = [["div", (t 1), (t 2)],
                   ["mflo", (t 0)]]

mipsRelOp :: String -> MIPSTerms
mipsRelOp "<"  = [["slt", (t 0), (t 1), (t 2)]]
mipsRelOp ">"  = [["slt", (t 0), (t 2), (t 1)]]
mipsRelOp "<=" = [["sle", (t 0), (t 1), (t 2)]]
mipsRelOp ">=" = [["sle", (t 0), (t 2), (t 1)]]
mipsRelOp "==" = [["seq", (t 0), (t 1), (t 2)]]
mipsRelOp "!=" = [["sne", (t 0), (t 1), (t 2)]]

insertNL :: [String] -> String
insertNL = concat . intersperse "\n"

insertWS :: [String] -> String
insertWS (i:is) = if length i > 0 && last i == ':'
                  then i
                  else concat ["    ", i, " ", concat (intersperse ", " is)]

v :: Int -> String
v n = "$v" ++ show n

a :: Int -> String
a n = "$a" ++ show n

t :: Int -> String
t n = "$t" ++ show n

zero :: String
zero = "$zero"



{- ***************************
         Assign Address
   *************************** -}

type FPReg   = Int
type GPReg   = Int
type AddrEnv = State (FPReg, GPReg, [(IVar, MVar)])

assignRelAddr :: IProgram -> MProgram
assignRelAddr = runAddrEnv . assignIProgram

runAddrEnv :: AddrEnv a -> a
runAddrEnv s = evalState s (-wordSize, -wordSize, [])

wordSize :: Int
wordSize = 4

assignFpWords :: Int -> AddrEnv MVar
assignFpWords n = do
  (fp, gp, asc) <- get
  let newFp = fp - n * wordSize
  put (newFp, gp, asc) >> return (Fp $ newFp + wordSize)

assignGpWords :: Int -> AddrEnv MVar
assignGpWords n = do
  (fp, gp, asc) <- get
  let newGp = gp - n * wordSize
  put (fp, newGp, asc) >> return (Gp $ newGp + wordSize)

assignWord :: (Int -> AddrEnv MVar) -> AddrEnv MVar
assignWord f = f 1

assignFpWord :: AddrEnv MVar
assignFpWord = assignWord assignFpWords

assignGpWord :: AddrEnv MVar
assignGpWord = assignWord assignGpWords

getFp :: AddrEnv FPReg
getFp = do
  (fp, gp, asc) <- get
  return fp

resetFp :: Int -> AddrEnv ()
resetFp n = do
  (fp, gp, asc) <- get
  put (n, gp, deleteOnlyFp asc)

deleteOnlyFp :: [(IVar, MVar)] -> [(IVar, MVar)]
deleteOnlyFp = filter f
    where f (_, mvar) = case mvar of
                          (Fp _) -> False
                          (Gp _) -> True

setAddr :: (Int -> AddrEnv MVar) -> IVar -> AddrEnv MVar
setAddr awf ivar = do addr <- calcAddr (objCtype ivar)
                      insertAddr ivar addr
                      return addr
    where
      calcAddr :: CType -> AddrEnv MVar
      calcAddr ty = case ty of
                      (CInt)          -> assignWord awf
                      (CTemp)         -> assignWord awf
                      (CPointer ty)   -> calcAddr ty
                      (CArray _ size) -> awf $ (toInt size)
                      _               -> error "never happen - calcAddr"

assignArgs :: [IVar] -> AddrEnv [MVar]
assignArgs args = liftM fst (foldM f ([], 0) args)
    where f (acc, addr) ivar = do let addr' = Fp addr
                                  insertAddr ivar addr'
                                  return (addr':acc, addr+wordSize)

insertAddr :: IVar -> MVar -> AddrEnv ()
insertAddr ivar addr = do
  (fp, gp, asc) <- get
  put (fp, gp, ((ivar, addr):asc))

getAddr :: IVar -> AddrEnv MVar
getAddr ivar = do (fp, gp, asc) <- get
                  case lookup ivar asc of
                    (Just addr) -> return addr
                    Nothing     -> setAddr assignFpWords ivar

assignIProgram :: IProgram -> AddrEnv MProgram
assignIProgram = mapM assignIDecl

assignIDecl :: IDecl -> AddrEnv MDecl
assignIDecl (IVarDecl ivar)        = liftM MVarDecl (setAddr assignGpWords ivar)
assignIDecl (IFunDecl f args stmt) = do resetFp (-wordSize)
                                        args'     <- assignArgs args
                                        stmt'     <- mapM assignIStmt stmt
                                        frameSize <- getFp
                                        return (MFunDecl frameSize
                                                         (objName f)
                                                         args'
                                                         stmt')

assignIStmt :: ICode -> AddrEnv MCode
assignIStmt (ILabel lab)           = return $ MLabel lab
assignIStmt (ILi dest n)           = liftM2 MLi (getAddr dest) (return n)
assignIStmt (ILet dest src)        = liftM2 MLet (getAddr dest) (getAddr src)
assignIStmt (IAop op dst e1 e2)    = liftM3 (MAop op) (getAddr dst)
                                                      (getAddr e1)
                                                      (getAddr e2)
assignIStmt (IRelop op dest e1 e2) = liftM3 (MRelop op) (getAddr dest)
                                                        (getAddr e1)
                                                        (getAddr e2)
assignIStmt (IWrite dest src)      = liftM2 MWrite (getAddr dest) (getAddr src)
assignIStmt (IRead dest src)       = liftM2 MRead (getAddr dest) (getAddr src)
assignIStmt (IAddr dest ivar)      = liftM2 MAddr (getAddr dest)(getAddr ivar)
assignIStmt (IJumpTr cond label)   = liftM2 MJumpTr (getAddr cond) (return label)
assignIStmt (IJumpFls cond label)  = liftM2 MJumpFls (getAddr cond) (return label)
assignIStmt (IJump label)          = return $ MJump label
assignIStmt (ICall dest f args)    = liftM3 MCall (getAddr dest)
                                                  (return $ objName f)
                                                  (mapM getAddr args)
assignIStmt (IReturn ivar)         = liftM MReturn (getAddr ivar)
assignIStmt (IRetVoid)             = return MRetVoid
assignIStmt (IPrint ivar)          = liftM MPrint (getAddr ivar)

toInt :: Integer -> Int
toInt n = (fromIntegral n) :: Int
