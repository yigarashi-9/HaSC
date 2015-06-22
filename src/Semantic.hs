module Semantic where

import qualified Data.Map as M
import Text.Parsec.Pos
import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict

import AST
import Environment
import AnalyzedAST

semanticAnalyze :: Program -> A_Program
semanticAnalyze prog = evalState body M.empty
    where
      body = do collectGDecl prog
                ret <- analyze prog
                typeCheck ret >> return ret


{- analyze**

   オブジェクト情報を収集して, それらの情報を含んだ新たな木を生成する.
   検出するエラーはプロトタイプ宣言と変数宣言が不正な場合のみである. -}

analyze :: Program -> StateEnv A_Program
analyze prog = liftM concat (mapM analyzeEDecl prog)

analyzeEDecl :: EDecl -> StateEnv [A_EDecl]
analyzeEDecl (Decl p l)              = return $ (map (A_Decl . convVar p) l)
analyzeEDecl (FuncPrototype _ _ _ _) = return []
analyzeEDecl (FuncDef p  _ name args stmt)
    = do {
        let { parms = map convParm args; };
        a_stmt <- withEnv 1 (appendWithDupCheck p 1 parms)
                  (analyzeStmt 2 stmt);
        func   <- find p 0 name;
        return $ [A_Func p func parms a_stmt]; }

analyzeStmt :: Level -> Stmt -> StateEnv A_Stmt
analyzeStmt lev (CompoundStmt _ s)  = withEnv (lev+1) (return ())
                                      (liftM A_CompoundStmt (mapM (analyzeStmt $ lev+1) s))
analyzeStmt lev (DeclStmt p l)      = let info = (map (convVar p) l)
                                      in appendWithDupCheck p lev info
                                             >> (return $ A_DeclStmt info)
analyzeStmt _   (EmptyStmt _)       = return A_EmptyStmt
analyzeStmt lev (ExprStmt _ e)      = liftM A_ExprStmt (analyzeExpr lev e)
analyzeStmt lev s@(IfStmt _ _ _ _)  = analyzeIf   lev s
analyzeStmt lev s@(WhileStmt _ _ _) = analyzeWhile lev s
analyzeStmt lev (ReturnStmt p e)    = liftM (A_ReturnStmt p) (analyzeExpr lev e)
analyzeStmt lev (RetVoidStmt p)     = return $ A_RetVoidStmt p


analyzeIf :: Level -> Stmt -> StateEnv A_Stmt
analyzeIf lev (IfStmt p cond tr fls)
    = do {
        a_cond <- analyzeExpr lev cond;
        liftM2 (A_IfStmt p a_cond) (analyzeStmt lev tr) (analyzeStmt lev fls); }

analyzeWhile :: Level -> Stmt -> StateEnv A_Stmt
analyzeWhile lev (WhileStmt p cond body)
    = do {
        a_cond <- analyzeExpr lev cond;
        liftM (A_WhileStmt p a_cond) (analyzeStmt lev body); }

analyzeExpr :: Level -> Expr -> StateEnv A_Expr
analyzeExpr lev (AssignExpr p e1 e2)
    = liftM2 (A_AssignExpr p) (analyzeExpr lev e1) (analyzeExpr lev e2)
analyzeExpr lev (UnaryPrim p op e)
    = liftM (A_UnaryPrim p op) (analyzeExpr lev e)
analyzeExpr lev (BinaryPrim p op e1 e2)
    = liftM2 (A_BinaryPrim p op) (analyzeExpr lev e1) (analyzeExpr lev e2)
analyzeExpr lev (ApplyFunc p nm args)
    = liftM2 (A_ApplyFunc p) (find p lev nm) (mapM (analyzeExpr lev) args)
analyzeExpr lev (MultiExpr p es) = liftM A_MultiExpr (mapM (analyzeExpr lev) es)
analyzeExpr lev (Constant p n)   = return $ A_Constant n
analyzeExpr lev (IdentExpr p n)
    = do info <- find p lev n
         case info of
           (_, (Func, _))      -> error $ concat [show p, ": you cannot refer to func ", n]
           (_, (FuncProto, _)) -> error $ concat [show p, ": you cannot refer to func ", n]
           s -> return $ A_IdentExpr s


{- typeCheck -}
typeCheck :: A_Program -> StateEnv ()
typeCheck = mapM_ declTypeCheck

declTypeCheck :: A_EDecl -> StateEnv ()
declTypeCheck (A_Func p info args body)
    = case getRetType info of
        (Just ty) -> stmtTypeCheck ty body
        Nothing   -> error $ concat [show p, ": invalid function type ", fst info]
declTypeCheck _ = wellTyped

stmtTypeCheck :: CType -> A_Stmt -> StateEnv ()
stmtTypeCheck retTy = stmtTypeCheck'
    where
      stmtTypeCheck' :: A_Stmt -> StateEnv ()
      stmtTypeCheck' (A_EmptyStmt)        = wellTyped
      stmtTypeCheck' (A_ExprStmt e)       = exprTypeCheck e >> wellTyped
      stmtTypeCheck' (A_DeclStmt l)       = wellTyped
      stmtTypeCheck' (A_CompoundStmt s)   = mapM_ stmtTypeCheck' s
      stmtTypeCheck' (A_IfStmt p c tr fl) = ifTypeCheck p c tr fl
      stmtTypeCheck' (A_WhileStmt p c bd) = whileTypeCheck p c bd
      stmtTypeCheck' (A_ReturnStmt p e)   = returnTypeCheck p retTy e
      stmtTypeCheck' (A_RetVoidStmt p)
          = when (retTy /= CVoid) (error $ concat [show p, ": invalid return type"])

ifTypeCheck :: SourcePos -> A_Expr -> A_Stmt -> A_Stmt -> StateEnv ()
ifTypeCheck p cond tr fls = do cond_ty <- exprTypeCheck cond
                               if cond_ty == CInt
                               then wellTyped
                               else error $ show p ++ ": condifion of if must be Int"

whileTypeCheck :: SourcePos -> A_Expr -> A_Stmt -> StateEnv ()
whileTypeCheck p cond body = do cond_ty <- exprTypeCheck cond
                                if cond_ty == CInt
                                then wellTyped
                                else error $ show p ++ ": condifion of while must be Int"

returnTypeCheck :: SourcePos -> CType -> A_Expr -> StateEnv ()
returnTypeCheck p retTy e = do ty <- exprTypeCheck e
                               if retTy == ty
                               then wellTyped
                               else error $ concat [show p, ": must return ", show retTy]

exprTypeCheck :: A_Expr -> StateEnv CType
exprTypeCheck (A_AssignExpr p e1 e2)
    = do {
        checkAssignForm p e1;
        ty1 <- exprTypeCheck e1;
        ty2 <- exprTypeCheck e2;
        if ty1 == ty2 then return ty1 else typeDiffError p "="; }
exprTypeCheck (A_UnaryPrim p op e)
    = case op of
        "&" -> checkAddressReferForm p e >> unaryTypeCheck p op e CInt (CPointer CInt)
        "*" -> unaryTypeCheck p op e (CPointer CInt) CInt
exprTypeCheck (A_BinaryPrim p op e1 e2)
    | op `elem` ["&&", "||", "*", "/"]
        = do {
            ty1 <- exprTypeCheck e1;
            ty2 <- exprTypeCheck e2;
            if ty1 == CInt && ty2 == CInt
            then return CInt
            else typeSpecError p op CInt; }
    | op `elem` ["==", "!=", "<", "<=", ">", ">="]
        = do {
            ty1 <- exprTypeCheck e1;
            ty2 <- exprTypeCheck e2;
            if ty1 == ty2
            then return ty1
            else typeDiffError p op; }
    | op `elem` ["+", "-"]
        = do {
            ty1 <- exprTypeCheck e1;
            ty2 <- exprTypeCheck e2;
            case (ty1, ty2) of
              (CInt, CInt)          -> return CInt
              (CPointer ty, CInt) -> return $ CPointer ty
              (CArray ty _, CInt) -> return $ CPointer ty
              _                     -> error $ show p ++ ": bad pointer operands"; }
exprTypeCheck (A_ApplyFunc p info args)
    = do {
        argTypes <- mapM exprTypeCheck args;
        case snd info of
          (Func, CFun ty parms) -> if argTypes == parms
                                   then return ty
                                   else error $ show p ++ ": bad arguments"
          _  -> error $ show p ++ ": '" ++ fst info ++ "' is not function" }
exprTypeCheck (A_MultiExpr es) = liftM last (mapM exprTypeCheck es)
exprTypeCheck (A_Constant n)   = return CInt
exprTypeCheck (A_IdentExpr i)  = return $ getType i

unaryTypeCheck :: SourcePos -> String -> A_Expr -> CType -> CType -> StateEnv CType
unaryTypeCheck p op e srcTy retTy
    = do {
        ty <- exprTypeCheck e;
        if ty == srcTy then return retTy else typeSpecError p op srcTy; }



{- Utility -}

checkAddressReferForm :: SourcePos -> A_Expr -> StateEnv ()
checkAddressReferForm _ (A_IdentExpr _) = wellTyped
checkAddressReferForm p _ = error $ concat [show p, "invalid argument of '&'"]

checkAssignForm :: SourcePos -> A_Expr -> StateEnv()
checkAssignForm p (A_IdentExpr (_, (kind, ty)))
    = case (kind, ty) of
        (Var, (CArray _ _))  -> error $ concat [show p, " : invalid assign"]
        (Var, _)             -> wellTyped
        (Parm, (CArray _ _)) -> error $ concat [show p, " : invalid assign"]
        (Parm, _)            -> wellTyped
        (Func, _)            -> error $ concat [show p, " : invalid assign"]
        (FuncProto, _)       -> error $ concat [show p, " : invalid assign"]
checkAssignForm p (A_UnaryPrim _ "*" _) = wellTyped
checkAssignForm p _ = error $ concat [show p, " : invalid assign"]

-- 二つの引数の型が同じでなければならない場合の Type Error
typeDiffError :: SourcePos -> String -> a
typeDiffError p op = error $ concat [show p, ": Types of operands '",
                                     op, "' must be same"]

-- 二つの引数の型がある型でなければいけない場合の Type Error
typeSpecError :: SourcePos -> String -> CType -> a
typeSpecError p op ty = error $ concat [show p, ": Types of operands '",
                                       op, "' must be ", show ty]

wellTyped :: StateEnv ()
wellTyped = return ()
