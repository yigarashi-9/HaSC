module ASTtoIntermed where

import Control.Monad
import Control.Monad.State
import qualified Control.Monad.Trans.State as S
import Data.List

import Semantic
import ObjInfo
import AnalyzedAST
import IntermedSyntax


{- *****************************
          Utils for Env
   ***************************** -}

type VarNum    = Int
type LabelNum  = Int
type Reuseable = [VarNum]
type IREnv     = S.StateT (Reuseable, VarNum) (State LabelNum)

runEnv :: IREnv a -> a
runEnv v = evalState (S.evalStateT v ([], 0)) 0

freshVar :: IREnv IVar
freshVar = do
  (reuse, num) <- S.get
  newnum <- case reuse of
              []     -> do S.put ([], num+1)
                           return num
              (n:ns) -> do S.put (ns, num)
                           return n
  return $ makeVar newnum
    where makeVar n = Info (ObjInfo (("@" ++) . show $ n) Var CTemp (-1))

freshVars :: Int -> IREnv [IVar]
freshVars n = replicateM n freshVar

resetVars :: IREnv ()
resetVars = S.put ([], 0)

freshLabel :: IREnv Label
freshLabel = do n <- lift get
                lift (put (n+1))
                return $ "@label" ++ show n

freshLabels :: Int -> IREnv [Label]
freshLabels n = replicateM n freshLabel


collectUnuseVar :: IVar -> IREnv ()
collectUnuseVar (Info (ObjInfo ('@':varNum) _ _ _))
    = do (reuse, num) <- S.get
         when (not $ (read varNum) `elem` reuse) (put (sort $ (read varNum):reuse, num))
collectUnuseVar _ = return ()


collectUnuseVars :: [IVar] -> IREnv ()
collectUnuseVars = mapM_ collectUnuseVar

result :: [IVar] -> IVar
result [] = error "unexpected vars, sorry."
result l  = head l



{- *******************************
            Convert AST
   ******************************* -}

astToIntermed :: A_Program -> IProgram
astToIntermed prog = runEnv $ mapM convDecl prog

showIProgram :: IProgram -> String
showIProgram prog = concat $ intersperse "\n" (map show prog)

convDecl :: A_EDecl -> IREnv IDecl
convDecl (A_Decl var)              = return (IVarDecl $ Info var)
convDecl (A_Func _ var parms body)
    = do resetVars
         (_, stmts) <- convStmt body
         return $ IFunDecl (Info var) (map Info parms) stmts


convStmt :: A_Stmt -> IREnv ([IVar], [ICode])
convStmt (A_EmptyStmt)  = return ([], [])
convStmt (A_ExprStmt e) = convExpr e
convStmt (A_IfStmt _ cond tr fls)
    = do (varsCond,  stmtCond)  <- convExpr cond
         (varsTrue,  stmtTrue)  <- convStmt tr
         (varsFalse, stmtFalse) <- convStmt fls
         (lfls:lexit:[])    <- freshLabels 2
         return (varsCond ++ varsTrue ++ varsFalse,
                 stmtCond
                 ++ [IJumpFls (result varsCond) lfls]
                 ++ stmtTrue
                 ++ [IJump lexit,
                     ILabel lfls]
                 ++ stmtFalse
                 ++ [ILabel lexit])
convStmt (A_WhileStmt _ cond body)
    = do (varsCond, stmtCond) <- convExpr cond
         (varsBody, stmtBody) <- convStmt body
         (lloop:lexit:[])     <- freshLabels 2
         return (varsCond ++ varsBody,
                 [ILabel lloop]
                 ++ stmtCond
                 ++ [IJumpFls (result varsCond) lexit]
                 ++ stmtBody
                 ++ stmtCond
                 ++ [IJump lloop, ILabel lexit])
convStmt (A_ReturnStmt _ e)
    = do (vars, stmts) <- convExpr e
         return (vars, stmts ++ [IReturn (result vars)])
convStmt (A_RetVoidStmt _) = return ([], [IRetVoid])
convStmt (A_CompoundStmt stmts)
    = do (decls, stmts) <- foldM foldCmpdStmt ([], []) stmts
         collectUnuseVars decls
         return (decls, stmts)

foldCmpdStmt :: ([IVar], [ICode]) -> A_Stmt -> IREnv ([IVar], [ICode])
foldCmpdStmt (idecl, istmt) stmt
    = case stmt of
        (A_DeclStmt l) -> return (idecl ++ map Info l, istmt)
        otherStmt      -> do (vars, stmts) <- convStmt otherStmt
                             return (idecl ++ (extractTemp vars),
                                     istmt ++ stmts)

{- 残念ながらプログラム上で宣言された変数も convExpr の結果に含まれてしまう．
   CompoundStmt の decl に入ってしまうと困るので削除する．-}
extractTemp :: [IVar] -> [IVar]
extractTemp = filter ((== '@') . head . objName . info)


{- 与えられた Expr の最終的な結果が格納される変数を
   常に結果タプル第1要素の先頭に配置する．
   それによってあとの変換で変数を使用することができる．-}
convExpr :: A_Expr -> IREnv ([IVar], [ICode])
convExpr (A_AssignExpr _ dest src)
    = case dest of
        (A_UnaryPrim _ "*" dst) -> do (vars1, stmts1) <- convExpr dst
                                      (vars2, stmts2) <- convExpr src
                                      return (vars2 ++ vars1,
                                              stmts1 ++ stmts2
                                             ++ [IWrite (result vars1)  (result vars2)])
        (A_IdentExpr vdst) -> do (vars, stmts) <- convExpr src
                                 return (vars, stmts ++
                                         [ILet (Info vdst) (result vars)])
convExpr (A_UnaryPrim _ "*" e)
    = do dest <- freshVar
         (vars, stmts) <- convExpr e
         return (dest:vars, stmts ++ [IRead dest (result vars)])
convExpr (A_UnaryPrim _ "&" e)
    = do dest <- freshVar
         (vars, stmts) <- convExpr e
         return (dest:vars, stmts ++ [IAddr dest (result vars)])
convExpr (A_BinaryPrim _ op e1 e2)
    | op `elem` ["+", "-", "*", "/"]
        = if isPointer e1
          then pointerCalc op e1 e2
          else arithCalc op e1 e2
    | op `elem` ["<", ">", "<=", ">=", "==", "!="]
        = do (vars1, stmts1) <- convExpr e1
             (vars2, stmts2) <- convExpr e2
             dest            <- freshVar
             return (dest:(vars1 ++ vars2),
                     stmts1
                     ++ stmts2
                     ++ [IRelop op dest (result vars1) (result vars2)])
    | op == "&&"
        = do (vars1, stmts1)  <- convExpr e1
             (vars2, stmts2)  <- convExpr e2
             dest             <- freshVar
             (lfls:lexit:[])  <- freshLabels 2
             return (dest:(vars1 ++ vars2),
                     stmts1
                     ++ stmts2
                     ++ [IJumpFls (result vars1) lfls,
                         IJumpFls (result vars2) lfls,
                         ILet dest (Const 1),
                         IJump lexit,
                         ILabel lfls,
                         ILet dest (Const 0),
                         ILabel lexit])
    | op == "||"
        = do (vars1, stmts1) <- convExpr e1
             (vars2, stmts2) <- convExpr e2
             dest            <- freshVar
             (ltr:lexit:[])  <- freshLabels 2
             return (dest:(vars1 ++ vars2),
                     stmts1
                     ++ stmts2
                     ++ [IJumpTr (result vars1) ltr,
                         IJumpTr (result vars1) ltr,
                         ILet dest (Const 0),
                         IJump lexit,
                         ILabel ltr,
                         ILet dest (Const 1),
                         ILabel lexit])
convExpr (A_ApplyFunc _ func args)
    = do res <- mapM convExpr args
         if objName func == "print"
         then let [(vars, stmts)] = res
              in return (vars, stmts ++ [IPrint (result vars)])
         else do dest <- freshVar
                 let resArgs = map (head . fst) res
                     resVars = concat $ map fst res
                     resStmt = concat $ map snd res
                 return (dest:resVars, resStmt ++ [ICall dest (Info func) resArgs])
convExpr (A_MultiExpr es)
    = do res <- mapM convExpr es
         -- 最後の結果が先頭にならないといけないので reverse
         return (concat . reverse $ map fst res, concat $ map snd res)
convExpr (A_Constant n)
    = freshVar >>= (\dest -> return ([dest], [ILet dest (Const n)]))
convExpr (A_IdentExpr i) = if isArray i
                           then do v <- freshVar
                                   return $ (v:[Info i], [IAddr v (Info i)])
                           else return ([Info i], [])

isArray :: ObjInfo -> Bool
isArray info = case objCtype info of
                 (CArray _ _) -> True
                 _            -> False

pointerCalc :: String -> A_Expr -> A_Expr -> IREnv ([IVar], [ICode])
pointerCalc op e1 e2
    = do (vars1, stmts1) <- convExpr e1
         (vars2, stmts2) <- convExpr e2
         (dest:v1:v2:[]) <- freshVars 3
         return (dest:v1:v2:(vars1 ++ vars2), stmts1 ++ stmts2 ++
                 [ILet v1 (Const 4),
                  IAop "*" v2   v1             (result vars2),
                  IAop op  dest (result vars1) v2])


arithCalc :: String -> A_Expr -> A_Expr -> IREnv ([IVar], [ICode])
arithCalc op e1 e2
    = do (vars1, stmts1) <- convExpr e1
         (vars2, stmts2) <- convExpr e2
         dest            <- freshVar
         return (dest:(vars1 ++ vars2), stmts1 ++ stmts2 ++
                 [IAop op dest (result vars1) (result vars2)])
