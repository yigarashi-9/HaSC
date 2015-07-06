module ASTtoIntermed where

import Control.Monad
import Control.Monad.State.Strict
import Data.List

import AnalyzedAST
import IntermedSyntax

type VarNum    = Int
type Reuseable = [VarNum]
type VarEnv    = State (Reuseable, VarNum)

runEnv :: VarEnv a -> a
runEnv v = evalState v ([], 0)

freshVar :: VarEnv IVar
freshVar = do
  (reuse, num) <- get
  newnum <- case reuse of
              []     -> do put ([], num+1)
                           return num
              (n:ns) -> do put (ns, num)
                           return n
  return $ makeVar newnum
    where makeVar n = (("@" ++) . show $ n, ObjInfo Var CTemp (-1))

freshVars :: Int -> VarEnv [IVar]
freshVars n = replicateM n freshVar

resetVars :: VarEnv ()
resetVars = put ([], 0)

collectUnuseVar :: IDecl -> VarEnv ()
collectUnuseVar (IVarDecl (('@':varNum), _)) = do (reuse, num) <- get
                                                  put (sort $ (read varNum):reuse, num)
collectUnuseVar _ = return ()

collectUnuseVars :: [IDecl] -> VarEnv ()
collectUnuseVars = mapM_ collectUnuseVar

result :: [IVar] -> IVar
result [] = error "unexpected vars, sorry."
result l  = head l

astToIntermed :: A_Program -> IProgram
astToIntermed prog = runEnv $ mapM convDecl prog

showIProgram :: IProgram -> String
showIProgram prog = concat $ intersperse "\n" (map show prog)

convDecl :: A_EDecl -> VarEnv IDecl
convDecl (A_Decl var)              = return (IVarDecl var)
convDecl (A_Func _ var parms body) = do (_, stmts) <- resetVars >> convStmt body
                                        let [cmpdStmt] = stmts
                                        return $ IFunDecl var parms cmpdStmt


convStmt :: A_Stmt -> VarEnv ([IVar], [IStmt])
convStmt (A_EmptyStmt)  = return ([], [IEmpty])
convStmt (A_ExprStmt e) = convExpr e
convStmt (A_DeclStmt _) = error "unexpected A_DeclStmt"
convStmt (A_IfStmt _ cond tr fls)
    = do (varsCond, makeCond)   <- convExpr cond
         (varsTrue, stmtTrue)   <- convStmt tr
         (varsFalse, stmtFalse) <- convStmt fls
         return (varsCond ++ varsTrue ++ varsFalse,
                 makeCond ++ [IIf (result varsCond) stmtTrue stmtFalse])
convStmt (A_WhileStmt _ cond body)
    = do (varsCond, makeCond)   <- convExpr cond
         (varsBody, stmtBody)   <- convStmt body
         return (varsCond ++ varsBody,
                 makeCond ++ [IWhile (result varsCond) stmtBody] ++ makeCond)
convStmt (A_ReturnStmt _ e)
    = do (vars, stmts) <- convExpr e
         return (vars, stmts ++ [IReturn (result vars)])
convStmt (A_RetVoidStmt _) = return ([], [IEmpty])
convStmt (A_CompoundStmt stmts)
    = do (decls, stmts) <- foldM foldCmpdStmt ([], []) stmts
         collectUnuseVars decls
         return ([], [ICompound (map head . group . sort $ decls) stmts])

foldCmpdStmt :: ([IDecl], [IStmt]) -> A_Stmt -> VarEnv ([IDecl], [IStmt])
foldCmpdStmt (idecl, istmt) stmt
    = case stmt of
        (A_DeclStmt l) -> return (idecl ++ map IVarDecl l, istmt)
        otherStmt      -> do (vars, stmts) <- convStmt otherStmt
                             return (idecl ++ map IVarDecl (extractTemp vars),
                                     istmt ++ stmts)

extractTemp :: [IVar] -> [IVar]
extractTemp = filter ((== '@') . head . fst)

convExpr :: A_Expr -> VarEnv ([IVar], [IStmt])
convExpr (A_AssignExpr _ dest src)
    = case dest of
        (A_UnaryPrim _ "*" dst) -> do (vars1, stmts1) <- convExpr dst
                                      (vars2, stmts2) <- convExpr src
                                      return (vars1 ++ vars2,
                                              stmts1 ++ stmts2
                                             ++ [IWrite (result vars1)  (result vars2)])
        (A_IdentExpr vdst) -> do (vars, stmts) <- convExpr src
                                 return (vars,
                                         stmts ++ [ILet vdst (IVarExp $ result vars)])
convExpr (A_UnaryPrim _ "*" e)
    = do dest <- freshVar
         (vars, stmts) <- convExpr e
         return (dest:vars, stmts ++ [IRead dest (result vars)])
convExpr (A_UnaryPrim _ "&" e)
    = do dest <- freshVar
         (vars, stmts) <- convExpr e
         return (dest:vars, stmts ++ [ILet dest (IAddr $ result vars)])
-- ポインタ演算はどうにかしないと
convExpr (A_BinaryPrim _ op e1 e2)
    | op `elem` ["+", "-", "*", "/"]
        = do (vars1, stmts1) <- convExpr e1
             (vars2, stmts2) <- convExpr e2
             dest            <- freshVar
             return (dest:(vars1 ++ vars2), stmts1 ++ stmts2 ++
                             [ILet dest $ IAop op (result vars1) (result vars2)])
    | op `elem` ["<", ">", "<=", ">=", "==", "!="]
        = do (vars1, stmts1) <- convExpr e1
             (vars2, stmts2) <- convExpr e2
             dest            <- freshVar
             return (dest:(vars1 ++ vars2), stmts1 ++ stmts2 ++
                             [ILet dest $ IRelop op (result vars1) (result vars2)])
    | op == "&&"
        = do (vars1, stmts1) <- convExpr e1
             (vars2, stmts2) <- convExpr e2
             dest            <- freshVar
             return (dest:(vars1 ++ vars2), stmts1 ++ stmts2 ++
                     [IIf (result vars1) [IIf (result vars2) [ILet dest (IInt 1)]
                                                             [ILet dest (IInt 0)]]
                                         [ILet dest (IInt 0)]])
    | op == "||"
        = do (vars1, stmts1) <- convExpr e1
             (vars2, stmts2) <- convExpr e2
             dest            <- freshVar
             return (dest:(vars1 ++ vars2), stmts1 ++ stmts2 ++
                     [IIf (result vars1) [ILet dest (IInt 1)]
                                         [IIf (result vars2) [ILet dest (IInt 1)]
                                                             [ILet dest (IInt 0)]]])
convExpr (A_ApplyFunc _ func args)
    = do dest <- freshVar
         res  <- mapM convExpr args
         let resArgs = map (head . fst) res
             resVars = concat $ map fst res
             resStmt = concat $ map snd res
         return (dest:resVars, resStmt ++ [ICall dest func resArgs])
convExpr (A_MultiExpr es)
    = do res <- mapM convExpr es
         -- 最後の結果が先頭にならないといけないので reverse
         return (concat . reverse $ map fst res, concat $ map snd res)
convExpr (A_Constant n)
    = freshVar >>= (\dest -> return ([dest], [ILet dest (IInt n)]))
convExpr (A_IdentExpr i) = return ([i], [])
