module GenCode where

import Data.Maybe
import Data.List
import Control.Monad.State

import AnalyzedAST
import IntermedSyntax

type MIPSCode = String
type Label    = String
type LabelEnv = State (Int, Int)


genCode :: IProgram -> MIPSCode
genCode prog = header ++ body
    where body = runLabelEnv (do assembly <- mapM genIDecl prog
                                 return $ insertNL assembly)
          header = "    .text\n    .globl main\n"

runLabelEnv :: LabelEnv a -> a
runLabelEnv s = evalState s (0, 0)

freshLabel :: LabelEnv Label
freshLabel = do (n, a) <- get
                put (n+1, a)
                return $ "label" ++ show n

setFrameSize :: Int -> LabelEnv ()
setFrameSize size = do (n, _) <- get
                       put (n, size)

getCurFrameSize :: LabelEnv Int
getCurFrameSize = liftM snd get

freshLabels :: Int -> LabelEnv [Label]
freshLabels n = replicateM n freshLabel

genIDecl :: IDecl -> LabelEnv MIPSCode
genIDecl (IFunDecl f args body)
    = do let fpSize = (length args) * wordSize
             size   = - (getFuncFrameSize body) + fpSize + wordSize * 2
         setFrameSize (-size)
         stmts <- genStmt body
         return (insertNL $ [ivarName f ++ ":"]
                         ++ [insertWS ["move", (t 0), "$sp"],
                             insertWS ["addi", "$sp", "$sp", show (-size)],
                             insertWS ["sw", "$fp", "0($sp)"],
                             insertWS ["sw", "$ra", "4($sp)"],
                             insertWS ["addi", "$fp", (t 0), show (-fpSize)]]
                         ++ stmts
                         ++ [insertWS ["lw", "$ra", "4($sp)"],
                             insertWS ["lw", "$fp", "0($sp)"],
                             insertWS ["addi", "$sp", "$sp", show size],
                             insertWS ["jr", "$ra"]])
genIDecl _ = return ""


getFuncFrameSize :: IStmt -> Int
getFuncFrameSize (IIf _ tr fls)  = minimum $ map getFuncFrameSize (tr ++ fls)
getFuncFrameSize (IWhile _ body) = minimum $ map getFuncFrameSize body
getFuncFrameSize (ICompound decl body)
    = minimum $ (map getFpAddr decl) ++ (map getFuncFrameSize body)
getFuncFrameSize _ = 0


genStmt :: IStmt -> LabelEnv [MIPSCode]
genStmt (IEmpty)          = return [""]
genStmt (ILet dest src)   = do exprs <- genExpr src
                               return $ exprs ++ [insertWS ["sw", (t 0), show dest]]
genStmt (IWrite dest src) = return [insertWS ["lw", (t 1), show src],
                                    insertWS ["lw", (t 0), show dest],
                                    insertWS ["sw", (t 1), "0(" ++ (t 0) ++ ")"]]
genStmt (IRead dest src)  = return [insertWS ["lw", (t 1), show src],
                                    insertWS ["lw", (t 0), "0(" ++ (t 1) ++ ")"],
                                    insertWS ["sw", (t 0), show dest]]
genStmt (IIf cond tr fls)
    = do (els:exit:[]) <- freshLabels 2
         trStmt <- liftM concat (mapM genStmt tr)
         flStmt <- liftM concat (mapM genStmt fls)
         return $ [insertWS ["lw", (t 0), show cond],
                   insertWS ["beq", (t 0), zero, els]]
                  ++ trStmt ++ [insertWS ["j", exit]]
                  ++ [els ++ ":"] ++ flStmt ++ [exit ++ ":"]
genStmt (IWhile cond body)
    = do (loop:exit:[]) <- freshLabels 2
         stmt           <- liftM concat (mapM genStmt body)
         return $ [loop ++ ":"]
                  ++ [insertWS ["lw", (t 0), show cond],
                      insertWS ["beq", (t 0), zero, exit]]
                  ++ stmt ++ [insertWS ["j", loop]] ++ [exit ++ ":"]
genStmt (ICall dest f args)
    = return $ setArgs args ++ [insertWS ["jal", ivarName f],
                                insertWS ["sw", (v 0), show dest]]
genStmt (IReturn ivar)
    = do frameSize <- getCurFrameSize
         return $ [insertWS ["lw", (v 0), show ivar],
                   insertWS ["lw", "$ra", "4($sp)"],
                   insertWS ["lw", "$fp", "0($sp)"],
                   insertWS ["addi", "$sp", "$sp", show (-frameSize)],
                   insertWS ["jr", "$ra"]]
genStmt (IRetVoid)
    = do frameSize <- getCurFrameSize
         return $ [insertWS ["lw", "$ra", "4($sp)"],
                   insertWS ["lw", "$fp", "0($sp)"],
                   insertWS ["addi", "$sp", "$sp", show (-frameSize)],
                   insertWS ["jr", "$ra"]]
genStmt (IPrint ivar)      = return $ [insertWS ["li", (v 0), show 1],
                                       insertWS ["lw", (a 0), show ivar],
                                       insertWS ["syscall"]]
genStmt (ICompound _ body) = liftM concat (mapM genStmt body)


setArgs :: [IVar] -> [MIPSCode]
setArgs = fst . foldr foldArgs ([], -4)
    where
      foldArgs :: IVar -> ([MIPSCode], Int) -> ([MIPSCode], Int)
      foldArgs arg (acc, addr) = ([insertWS ["lw", (t 0), show arg],
                                   insertWS ["sw", (t 0), show addr ++ "($sp)"]] ++ acc,
                                  addr - wordSize)

genExpr :: IExpr -> LabelEnv [String]
genExpr (IVarExp ivar)    = return [insertWS ["lw", (t 0), show ivar]]
genExpr (IInt n)          = return [insertWS ["li", (t 0), show n]]
genExpr (IAop op e1 e2)   = return $ [insertWS ["lw", (t 1), show e1],
                                      insertWS ["lw", (t 2), show e2]] ++ mipsArithOp op
genExpr (IRelop op e1 e2)
    = do relop <- mipsRelOp op
         return $ [insertWS ["lw", (t 1), show e1],
                   insertWS ["lw", (t 2), show e2]] ++ relop
genExpr(IAddr (Addr a))   = case a of
                              (Fp n) -> return [insertWS ["addi", (t 0), "$fp", show n]]
                              (Gp n) -> return [insertWS ["addi", (t 0), "$gp", show n]]
                              _      -> undefined


mipsArithOp :: String -> [String]
mipsArithOp "+" = [insertWS ["add", (t 0), (t 1), (t 2)]]
mipsArithOp "-" = [insertWS ["sub", (t 0), (t 1), (t 2)]]
mipsArithOp "*" = [insertWS ["mult", (t 1), (t 2)],
                   insertWS ["mflo", (t 0)]]
mipsArithOp "/" = [insertWS ["div", (t 1), (t 2)],
                   insertWS ["mflo", (t 0)]]

mipsRelOp :: String -> LabelEnv [String]
mipsRelOp "<"  = return $ [insertWS ["slt", (t 0), (t 1), (t 2)]]
mipsRelOp ">"  = return $ [insertWS ["slt", (t 0), (t 2), (t 1)]]
mipsRelOp "<=" = return $ [insertWS ["addi", (t 2), show 1],
                           insertWS ["slt", (t 0), (t 1), (t 2)]]
mipsRelOp ">=" = return $ [insertWS ["addi", (t 1), show 1],
                           insertWS ["slt", (t 0), (t 2), (t 1)]]
mipsRelOp "==" = do (l0:l1:[]) <- freshLabels 2
                    return $ [insertWS ["beq", (t 1), (t 2), l0],
                              insertWS ["li", (t 0), show 0],
                              insertWS ["j", l1],
                              l0 ++ ":",
                              insertWS ["li", (t 0), show 1],
                              l1 ++ ":"]
mipsRelOp "!=" = do (l0:l1:[]) <- freshLabels 2
                    return $ [insertWS ["bne", (t 1), (t 2), l0],
                              insertWS ["li", (t 0), show 0],
                              insertWS ["j", l1],
                              l0 ++ ":",
                              insertWS ["li", (t 0), show 1],
                              l1 ++ ":"]

insertNL :: [String] -> String
insertNL = concat . intersperse "\n"

insertWS :: [String] -> String
insertWS (i:is) = concat ["    ", i, " ", concat (intersperse ", " is)]

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
type IVarAddr = IVar
type AddrEnv = State (FPReg, GPReg, [(IVar, IVarAddr)])

assignRelAddr :: IProgram -> IProgram
assignRelAddr = runAddrEnv . assignIProgram

runAddrEnv :: AddrEnv a -> a
runAddrEnv s = evalState s (-wordSize, -wordSize, [])

wordSize :: Int
wordSize = 4

assignFpWords :: Int -> AddrEnv IVarAddr
assignFpWords n = do
  (fp, gp, asc) <- get
  let newFp = fp - n * wordSize
  put (newFp, gp, asc) >> return (Addr $ Fp (newFp + wordSize))

assignGpWords :: Int -> AddrEnv IVarAddr
assignGpWords n = do
  (fp, gp, asc) <- get
  let newGp = gp - n * wordSize
  put (fp, newGp, asc) >> return (Addr $ Gp (newGp + wordSize))

assignWord :: (Int -> AddrEnv IVarAddr) -> AddrEnv IVarAddr
assignWord f = f 1

assignFpWord :: AddrEnv IVarAddr
assignFpWord = assignWord assignFpWords

assignGpWord :: AddrEnv IVarAddr
assignGpWord = assignWord assignGpWords

getFp :: AddrEnv FPReg
getFp = do
  (fp, gp, asc) <- get
  return fp

resetFp :: Int -> AddrEnv ()
resetFp n = do
  (fp, gp, asc) <- get
  put (n, gp, asc)

setAddr :: (Int -> AddrEnv IVarAddr) -> IDecl -> AddrEnv IVarAddr
setAddr awf (IVarDecl ivar) = do addr <- calcAddr (ivarType ivar)
                                 insertAddr ivar addr
                                 return addr
    where
      calcAddr :: CType -> AddrEnv IVarAddr
      calcAddr ty = case ty of
                      (CInt)          -> assignWord awf
                      (CTemp)         -> assignWord awf
                      (CPointer ty)   -> calcAddr ty
                      (CArray _ size) -> awf $ (toInt size)
                      _               -> error "never happen - calcAddr"

assignArgs :: [IVar] -> AddrEnv [IVarAddr]
assignArgs args = liftM fst (foldM f ([], 0) args)
    where f (acc, addr) ivar = do let addr' = Addr $ Fp addr
                                  insertAddr ivar addr'
                                  return (addr':acc, addr+wordSize)

insertAddr :: IVar -> IVarAddr -> AddrEnv ()
insertAddr ivar addr = do
  (fp, gp, asc) <- get
  put (fp, gp, ((ivar, addr):asc))

getAddr :: IVar -> AddrEnv IVarAddr
getAddr ivar = do
  (fp, gp, asc) <- get
  return (fromJust $ lookup ivar asc)

assignIProgram :: IProgram -> AddrEnv IProgram
assignIProgram = mapM assignIDecl

assignIDecl :: IDecl -> AddrEnv IDecl
assignIDecl i@(IVarDecl _)         = liftM IVarDecl (setAddr assignGpWords i)
assignIDecl (IFunDecl f args stmt) = do resetFp (-wordSize)
                                        assignArgs args
                                        stmt' <- assignIStmt stmt
                                        return (IFunDecl f args stmt')

assignIStmt :: IStmt -> AddrEnv IStmt
assignIStmt (IEmpty)              = return IEmpty
assignIStmt (ILet dest src)       = liftM2 ILet (getAddr dest) (assignIExpr src)
assignIStmt (IWrite dest src)     = liftM2 IWrite (getAddr dest) (getAddr src)
assignIStmt (IRead dest src)      = liftM2 IRead (getAddr dest) (getAddr src)
assignIStmt (IIf cond tr fls)     = liftM3 IIf (getAddr cond)
                                               (mapM assignIStmt tr)
                                               (mapM assignIStmt fls)
assignIStmt (IWhile cond bd)      = liftM2 IWhile (getAddr cond) (mapM assignIStmt bd)
assignIStmt (ICall dest f args)   = liftM3 ICall (getAddr dest)
                                                 (return f)
                                                 (mapM getAddr args)
assignIStmt (IReturn ivar)        = liftM IReturn (getAddr ivar)
assignIStmt (IRetVoid)            = return IRetVoid
assignIStmt (IPrint ivar)         = liftM IPrint (getAddr ivar)
assignIStmt (ICompound decl stmt) = do fp <- getFp
                                       decl' <- mapM (setAddr assignFpWords) decl
                                       stmt' <- mapM assignIStmt stmt
                                       resetFp fp
                                       return (ICompound (map IVarDecl decl') stmt')

assignIExpr :: IExpr -> AddrEnv IExpr
assignIExpr (IVarExp ivar)    = liftM IVarExp (getAddr ivar)
assignIExpr (IAop op e1 e2)   = liftM2 (IAop op) (getAddr e1) (getAddr e2)
assignIExpr (IRelop op e1 e2) = liftM2 (IRelop op) (getAddr e1) (getAddr e2)
assignIExpr (IAddr ivar)      = liftM IAddr (getAddr ivar)
assignIExpr n                 = return n

toInt :: Integer -> Int
toInt n = (fromIntegral n) :: Int
