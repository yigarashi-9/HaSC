module HaSC.NASM.GenCode where

import Control.Monad
import Control.Monad.State
import Data.List

import HaSC.Prim.ObjInfo
import HaSC.Prim.IntermedSyntax
import HaSC.NASM.IntermedSyntax

type NASMCode = String

nasmGenCode :: NProgram -> NASMCode
nasmGenCode prog = concat $ intersperse "\n"
                   (genGlobalVarDecl prog ++ concatMap genFuncDecl prog)

genGlobalVarDecl :: NProgram -> [NASMCode]
genGlobalVarDecl prog = ["extern printf",
                         "section .data"]
                        ++ ["fmt: db \"%d\", 10, 0"]
                        ++ var
                        ++ ["section .bss"]
                        ++ seq
                        ++ ["section .text",
                            "global main"]
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
    = [name ++ ":"]
      ++ (map insertComma $
          [["push", ebp],
           ["mov", ebp, esp],
           ["sub", esp, show nlocal]]
         ++ concatMap genStmt body
         ++ [["mov", esp, ebp],
             ["pop", ebp],
             ["ret"]])
genFuncDecl _ = [""]

genStmt :: NCode -> [[NASMCode]]
genStmt (NLabel label)      = [[label ++ ":"]]
genStmt (NLi dest n)        = [["mov", show dest, dword n]]
genStmt (NLet dest src)     = [["mov", eax, show src],
                               ["mov", show dest, eax]]
genStmt (NAop op d e1 e2)
    = case op of
        "/" -> [["mov", eax, show e1],
                 ["mov", edx, dword 0],
                 ["mov", ebx, show e2],
                 ["div", ebx],
                 ["mov", show d, eax]]
        _    -> [["mov", eax, show e1],
                 ["mov", ebx, show e2],
                 [nasmArithOp op, eax, ebx],
                 ["mov", show d, eax]]
genStmt (NRelop op d e1 e2) = [["mov", eax, show e1],
                               ["cmp", eax, show e2],
                               [nasmRelOp op, "al"],
                               ["movzx", eax, "al"],
                               ["mov", show d, eax]]
genStmt (NWrite dest src)   = [["mov", eax, show src],
                               ["mov", ebx, show dest],
                               ["mov", "[" ++ ebx ++ "]", eax]]
genStmt (NRead dest src)    = [["mov", eax, show src],
                               ["mov", eax, "[", eax, "]"],
                               ["mov", show dest, eax]]
genStmt (NAddr dest src)    = (case src of
                                 Bp n   -> [["mov", eax, ebp],
                                            ["add", eax, show n]]
                                 Gb v _ -> [["lea", eax, "[", v, "]"]])
                              ++ [["mov", show dest, eax]]
genStmt (NJumpTr cond lab)  = [["cmp", show cond, dword 0],
                               ["jne", lab]]
genStmt (NJumpFls cond lab) = [["cmp", show cond, dword 0],
                               ["je", lab]]
genStmt (NJump lab)         = [["jmp", lab]]
genStmt (NCall dest f args) = concatMap (\a -> [["mov", eax, show a],
                                                ["push", eax]]) args
                              ++ [["call", f],
                                  ["add", esp, show $ (length args) * 4],
                                  ["mov", show dest, eax]]
genStmt (NReturn ivar)      = [["mov", eax, show ivar],
                               ["mov", esp, ebp],
                               ["pop", ebp],
                               ["ret"]]
genStmt (NRetVoid)          = [["mov", esp, ebp],
                               ["pop", ebp],
                               ["ret"]]
genStmt (NPrint ivar)       = [["mov", eax, show ivar],
                               ["push", eax],
                               ["push", "dword fmt"],
                               ["call", "printf"],
                               ["add", esp, show 8]]

nasmArithOp :: String -> String
nasmArithOp op = case op of
                   "+" -> "add"
                   "-" -> "sub"
                   "*" -> "imul"

nasmRelOp :: String -> String
nasmRelOp op = case op of
                 ">"  -> "setg"
                 ">=" -> "setge"
                 "<"  -> "setl"
                 "<=" -> "setle"
                 "==" -> "sete"
                 "!=" -> "setne"

insertComma :: [String] -> String
insertComma [i]        = i
insertComma (i0:i1:[]) = i0 ++ " " ++ i1
insertComma (i0:i1:is) = i0 ++ " " ++ i1 ++ ", " ++ tailTerm is
    where tailTerm []  = ""
          tailTerm [i] = i
          tailTerm is  = concat is

dword :: (Num a, Show a) => a -> String
dword n = "dword " ++ show n

ebp = "ebp"
esp = "esp"
eax = "eax"
ebx = "ebx"
ecx = "ecx"
edx = "edx"

{- assigAddr -}
type RBP     = Int
type AddrEnv = State (RBP, [(IVar, NVar)])

wordSize :: Int
wordSize = 4

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
                          (Bp _)   -> False
                          (Gb _ _) -> True

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
                                        body'  <- mapM assignIStmt body
                                        nlocal <- liftM fst get
                                        return $ NFunDecl (-nlocal-wordSize)
                                                          (objName f)
                                                          body'

assignArgs :: [IVar] -> AddrEnv [NVar]
assignArgs args = liftM fst (foldM f ([], wordSize * 2) args)
    where f (acc, addr) ivar = do let addr' = Bp addr
                                  insertAddr ivar addr'
                                  return (addr':acc, addr+wordSize)

assignIStmt :: ICode -> AddrEnv NCode
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
