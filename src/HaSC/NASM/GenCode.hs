module HaSC.NASM.GenCode where

import Control.Monad
import Control.Monad.State

import HaSC.Prim.ObjInfo
import HaSC.Prim.IntermedSyntax
import HaSC.NASM.IntermedSyntax

type NASMCode = String

nasmGenCode :: NProgram -> NASMCode
nasmGenCode prog = concat $ intersperse "\n"
                   (genGlobalVarDecl prog ++ map genFuncDecl prog)

genGlobalVarDecl :: NProgram -> [NASMCode]
genGlobalVarDecl prog = ["section .data"]
                        ++ var
                        ++ ["\n", "section .bss"]
                        ++ seq
    where
      (var, seq) = foldr divideVarSeq ([], []) prog

      divideVarSeq :: NDecl -> ([NASMCode], [NASMCode]) -> ([NASMCode], [NASMCode])
      divideVarSeq (NVarDecl (Gb name ty)) (var, seq)
          = case ty of
              (CArray _ n) -> (var, (name ++ ":  resb " ++ show (n*4)):seq)
              _            -> ((name ++ ":"):var, seq)
      divideVarSeq _ acc = acc

genFuncDecl :: NDecl -> [NASMCode]
genFuncDecl (NFunDecl nlocal name body)
    = [name]
      ++ map insertComma $
         [["push", "rbp"],
          ["mov", "rbp", "rsp"],
          ["sub", "rsp", show nlocal]]
         ++ genStmt body
         ++ [["mov", "rsp", "rbp"],
             ["pop", "rbp"],
             ["ret"]]

genStmt :: NCode -> [[NASMCode]]
genStmt (NLabel label)      = [[label ++ ":"]]
genStmt (NLi dest n)        = [["mov", show dest, show n]]
genStmt (NLet dest src)     = [["mov", show dest, show src]]
-- まだ
genStmt (NAop op d e1 e2)
    = case op of
        "\\" -> [["mov", "rax", show e1],
                 ["mov", "rdx", show 0],
                 ["mov", "rbx", show e2],
                 ["div", "rbx"],
                 ["mov", show d, "rax"]]
        _    -> [["mov", (r 8), show e1],
                 ["mov", (r 9), show e2],
                 [nasmArithOp op, (r 8), (r 9)],
                 ["mov", show d, (r 8)]]
genStmt (NRelop op d e1 e2) = [["lw", (t 1), show e1],
                               ["lw", (t 2), show e2]]
                              ++ mipsRelOp op
                              ++ [["sw", (t 0), show d]]
-- ここまでまだ
genStmt (NWrite dest src)   = [["mov", show dest, show src]]
genStmt (NRead dest src)    = [["mov", show dest, show src]]
genStmt (NAddr dest src)    = (case src of
                                 Bp n   -> [["mov", (r 8), "rbp"],
                                            ["add", (r 8), show n]]
                                 Gp v _ -> [["lea", (r 8), v]])
                              ++ [["mov", show dest, (r 8)]]
genStmt (NJumpTr cond lab)  = [["cmp", show cond, "0"],
                               ["jne", lab]]
genStmt (NJumpFls cond lab) = [["cmp", show cond, "0"],
                               ["jeq", lab]]
genStmt (NJump lab)         = [["jmp", lab]]
genStmt (NCall dest f args) = map (\a -> ["push", show a]) args
                              ++ [["call", f],
                                  ["add", "rsp", show $ (length args) * 4],
                                  ["mov", show dest, "rax"]]
genStmt (NReturn ivar)      = [["mov", "rax", show ivar],
                               ["mov", "rsp", "rbp"],
                               ["pop", "rbp"],
                               ["ret"]]
genStmt (NRetVoid)          = [["mov", "rsp", "rbp"],
                               ["pop", "rbp"],
                               ["ret"]]
genStmt (NPrint ivar)       = [["mov", "rdi", "fmt"],
                               ["mov", "rsi", show ivar],
                               ["call", "printf"]]

nasmArithOp :: String -> String
nasmArithOp op = case op of
                   "+" -> "add"
                   "-" -> "sub"
                   "*" -> "imul"

r :: Int -> String
r n = "r" ++ show n

{- assigAddr -}
type RBP     = Int
type AddrEnv = State (RBP, [(IVar, NVar)])

wordSize :: Int
wordSize = -4

setGlobal :: IVar -> AddrEnv NVar
setGlobal ivar = do let nvar = Gb (objName ivar) (objCtype ivar)
                    (rbp, asc) <- get
                    put (rbp, (ivar, nvar):asc)
                    return nvar

resetRBP :: AddrEnv ()
resetRBP = do (rbp, asc) <- get
              put (0, deleteOnlyBp asc)

getAddr :: IVar -> AddrEnv NVar
getAddr ivar = do (_, asc) <- get
                  case lookup ivar asc of
                    (Just addr) -> return addr
                    Nothing     -> setAddr ivar

setAddr :: IVar -> AddrEnv NVar
setAddr ivar = do addr <- calcAddr (objCtype ivar)
                  insertAddr ivar addr
                  return addr
    where
      calcAddr :: CType -> AddrEnv NVar
      calcAddr ty = case ty of
                      (CInt)          -> assignWords 1
                      (CTemp)         -> assignWords 1
                      (CPointer ty)   -> calcAddr ty
                      (CArray _ size) -> assignWords (toInt size)
                      _               -> error "never happen - calcAddr"

assignWords :: Int -> AddrEnv NVar
assignWords n = do (rbp, asc) <- get
                   let newRbp = rbp - n * wordSize
                   put (newRbp, asc)
                   return $ Bp newRbp

insertAddr :: IVar -> NVar -> AddrEnv ()
insertAddr ivar nvar = do (rbp, asc) <- get
                          put (rbp, (ivar,nvar):asc)

deleteOnlyBp :: [(IVar, NVar)] -> [(IVar, NVar)]
deleteOnlyBp = filter f
    where f (_, nvar) = case nvar of
                          (Bp _) -> False
                          (Gb _) -> True

assignRelAddr :: IProgram -> NProgram
assignRelAddr = runAddrEnv . assignIProgram

runAddrEnv :: AddrEnv a -> a
runAddrEnv s = evalState s (0, [])

assignIProgram :: IProgram -> AddrEnv NProgram
assignIProgram = mapM assignIDecl

assignIDecl :: IDecl -> AddrEnv NDecl
assignIDecl (IVarDecl ivar)        = liftM NVarDecl (setGlobal ivar)
assignIDecl (IFunDecl f args body) = do resetRBP
                                        assignArgs (reverse args)
                                        stmt'  <- mapM assignIStmt stmt
                                        nlocal <- lift fst get
                                        return $ NFunDecl -(nlocal+wordSize)
                                                          (objName f)
                                                          stmt'

assignArgs :: [IVar] -> AddrEnv [NVar]
assignArgs args = liftM fst (foldM f ([], wordSize * 2) args)
    where f (acc, addr) ivar = do let addr' = Bp addr
                                  insertAddr ivar addr'
                                  return (addr':acc, addr+wordSize)

assignIStmt :: ICode -> AddrEnv MCode
assignIStmt (ILabel lab)           = return $ NLabel lab
assignIStmt (ILi dest n)           = liftM2 NLi (getAddr dest) (return n)
assignIStmt (ILet dest src)        = liftM2 NLet (getAddr dest) (getAddr src)
assignIStmt (IAop op dst e1 e2)    = liftM3 (NAop op) (getAddr dst)
                                                      (getAddr e1)
                                                      (getAddr e2)
assignIStmt (IRelop op dest e1 e2) = liftM3 (NRelop op) (getAddr dest)
                                                        (getAddr e1)
                                                        (getAddr e2)
assignIStmt (IWrite dest src)      = liftM2 NWrite (getAddr dest) (getAddr src)
assignIStmt (IRead dest src)       = liftM2 NRead (getAddr dest) (getAddr src)
assignIStmt (IAddr dest ivar)      = liftM2 NAddr (getAddr dest)(getAddr ivar)
assignIStmt (IJumpTr cond label)   = liftM2 NJumpTr (getAddr cond) (return label)
assignIStmt (IJumpFls cond label)  = liftM2 NJumpFls (getAddr cond) (return label)
assignIStmt (IJump label)          = return $ NJump label
assignIStmt (ICall dest f args)    = liftM3 NCall (getAddr dest)
                                                  (return $ objName f)
                                                  (mapM getAddr args)
assignIStmt (IReturn ivar)         = liftM NReturn (getAddr ivar)
assignIStmt (IRetVoid)             = return NRetVoid
assignIStmt (IPrint ivar)          = liftM NPrint (getAddr ivar)

toInt :: Integer -> Int
toInt n = (fromIntegral n) :: Int
