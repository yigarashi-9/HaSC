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
                             return (idecl ++ map IVarDecl vars, istmt ++ stmts)


convExpr :: A_Expr -> VarEnv ([IVar], [IStmt])
convExpr expr = freshVar >>= convExprWithVar expr


{- gVar (givenVar) が最終的な結果を格納するための変数．
   Stmtのレベルでそれぞれの Expr の結果が必要になる
   (例えば if の条件節で使う)ので， 格納先の変数を必ず
   リストの先頭につけて head で回収できるようにする．
   ただし，再帰的に convExprWithVar が呼び出される場合は
   その中でリストが完成するので必ずしも明示的に gVar を追加しない-}
convExprWithVar :: A_Expr -> IVar -> VarEnv ([IVar], [IStmt])
convExprWithVar (A_AssignExpr _ dest src) gVar
    = case dest of
        (A_UnaryPrim _ "*" dst) -> do vsrc <- freshVar
                                      (vars1, stmts1) <- convExprWithVar dst gVar
                                      (vars2, stmts2) <- convExprWithVar src vsrc
                                      return (vars1 ++ vars2,
                                              stmts1 ++ stmts2 ++ [IWrite gVar vsrc])
        (A_IdentExpr vdst)      -> do vsrc <- freshVar
                                      (vars, stmts) <- convExprWithVar src vsrc
                                      return (vars, stmts ++ [ILet vdst (IVarExp vsrc)])
convExprWithVar (A_UnaryPrim _ "*" e) gVar
    = do v <- freshVar
         (vars, stmts) <- convExprWithVar e v
         return (gVar:vars, stmts ++ [IRead gVar v])
convExprWithVar (A_UnaryPrim _ "&" e) gVar
    = do v <- freshVar
         (vars, stmts) <- convExprWithVar e v
         return (gVar:vars, stmts ++ [ILet gVar (IAddr v)])
-- ポインタ演算はどうにかしないと
convExprWithVar (A_BinaryPrim _ op e1 e2) gVar
    | op `elem` ["+", "-", "*", "/"]
        = do (v1:v2:[]) <- freshVars 2
             (vars1, stmts1) <- convExprWithVar e1 v1
             (vars2, stmts2) <- convExprWithVar e2 v2
             return (gVar:(vars1 ++ vars2),
                     stmts1 ++ stmts2 ++ [ILet gVar $ IAop op v1 v2])
    | op `elem` ["<", ">", "<=", ">=", "==", "!="]
        = do (v1:v2:[]) <- freshVars 2
             (vars1, stmts1) <- convExprWithVar e1 v1
             (vars2, stmts2) <- convExprWithVar e2 v2
             return (gVar:(vars1 ++ vars2),
                     stmts1 ++ stmts2 ++ [ILet gVar $ IRelop op v1 v2])
    | op == "&&"
        = do (v1:v2:[]) <- freshVars 2
             (vars1, stmts1) <- convExprWithVar e1 v1
             (vars2, stmts2) <- convExprWithVar e2 v2
             return (gVar:(vars1 ++ vars2),
                     stmts1 ++ stmts2 ++
                     [IIf v1 [IIf v2 [ILet gVar (IInt 1)]
                                     [ILet gVar (IInt 0)]]
                             [ILet gVar (IInt 0)]])
    | op == "||"
        = do (v1:v2:[]) <- freshVars 2
             (vars1, stmts1) <- convExprWithVar e1 v1
             (vars2, stmts2) <- convExprWithVar e2 v2
             return (gVar:(vars1 ++ vars2),
                     stmts1 ++ stmts2 ++
                     [IIf v1 [ILet gVar (IInt 1)]
                             [IIf v2 [ILet gVar (IInt 1)]
                                     [ILet gVar (IInt 0)]]])
convExprWithVar (A_ApplyFunc _ func args) gVar
    = let len = length args
      in do vargs <- freshVars len
            (vars, stmts) <- foldM foldConvExpr ([], []) (zip args vargs)
            return (gVar:vars, stmts ++ [ICall gVar func vargs])
convExprWithVar (A_MultiExpr es) gVar
    = do tmpVars <- freshVars ((length es) - 1)
         foldM foldConvExpr ([], []) (zip es (tmpVars ++ [gVar]))
convExprWithVar (A_Constant n) gVar  = return ([gVar], [ILet gVar (IInt n)])
convExprWithVar (A_IdentExpr i) gVar = return ([gVar], [ILet gVar (IVarExp i)])

foldConvExpr :: ([IVar], [IStmt]) -> (A_Expr, IVar) -> VarEnv ([IVar], [IStmt])
foldConvExpr (accVars, accStmts) (arg, gVar)
    = do (vars, stmts) <- convExprWithVar arg gVar
         return (vars ++ accVars, accStmts ++ stmts)
