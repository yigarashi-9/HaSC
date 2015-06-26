module Semantic where

import qualified Data.Map as M
import Text.Parsec.Pos
import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Control.Monad.State.Strict

import AST
import Environment
import AnalyzedAST
import ErrorMsg

semanticAnalyze :: Program -> (A_Program, [String])
semanticAnalyze prog = runEnv body M.empty
    where body = do collectGlobal prog
                    ret <- analyze prog
                    typeCheck ret >> return ret

{- analyze**

   オブジェクト情報を収集して, それらの情報を含んだ新たな木を生成する.
   検出するエラーはプロトタイプ宣言と変数宣言が不正な場合のみである. -}

analyze :: Program -> StateEnv A_Program
analyze prog = liftM concat (mapM analyzeEDecl prog)

analyzeEDecl :: EDecl -> StateEnv [A_EDecl]
analyzeEDecl (Decl p dcl_ty)
    = return $ map (A_Decl . makeVarInfo p global_lev) dcl_ty
analyzeEDecl (FuncPrototype _ _ _ _) = return []
analyzeEDecl (FuncDef p _ name args stmt)
    = do {
        let { parms = map (makeParmInfo p) args; };
        a_stmt <- withEnv param_lev
                          (mapM_ (extendEnv p param_lev) parms)
                          (analyzeStmt func_lev stmt);
        func   <- findFromJust p global_lev name;
        return $ [A_Func p (name, func) parms a_stmt]; }

analyzeStmt :: Level -> Stmt -> StateEnv A_Stmt
analyzeStmt lev (CompoundStmt _ stmts)
    = withEnv (lev+1)
              (return ())
              (liftM A_CompoundStmt (mapM (analyzeStmt $ lev+1) stmts))
analyzeStmt lev (DeclStmt p dcls)
    = let info = map (makeVarInfo p lev) dcls
      in mapM_ (extendEnv p lev) info >> (return $ A_DeclStmt info)
analyzeStmt _   (EmptyStmt _)       = return A_EmptyStmt
analyzeStmt lev (ExprStmt _ e)      = liftM A_ExprStmt (analyzeExpr lev e)
analyzeStmt lev s@(IfStmt _ _ _ _)  = analyzeIf lev s
analyzeStmt lev s@(WhileStmt _ _ _) = analyzeWhile lev s
analyzeStmt lev (ReturnStmt p e)    = liftM (A_ReturnStmt p) (analyzeExpr lev e)
analyzeStmt lev (RetVoidStmt p)     = return $ A_RetVoidStmt p


analyzeIf :: Level -> Stmt -> StateEnv A_Stmt
analyzeIf lev (IfStmt p cond true false)
    = do {
        a_cond <- analyzeExpr lev cond;
        liftM2 (A_IfStmt p a_cond) (analyzeStmt lev true) (analyzeStmt lev false); }

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
analyzeExpr lev (ApplyFunc p name args)
    = do {
        funcInfo <- findFromJust p lev name;
        a_args   <- mapM (analyzeExpr lev) args;
        return $ A_ApplyFunc p (name, funcInfo) a_args; }
analyzeExpr lev (MultiExpr p es) = liftM A_MultiExpr (mapM (analyzeExpr lev) es)
analyzeExpr lev (Constant p num)   = return $ A_Constant num
analyzeExpr lev (IdentExpr p name)
    = do {
        info <- findFromJust p lev name;
        case info of
          (ObjInfo Func _ _)
              -> funcReferError p name
          (ObjInfo FuncProto _ _)
              -> funcReferError p name
          validInfo -> return $ A_IdentExpr (name, validInfo); }


{- typeCheck -}
typeCheck :: A_Program -> StateEnv ()
typeCheck = mapM_ declTypeCheck

declTypeCheck :: A_EDecl -> StateEnv ()
declTypeCheck (A_Decl _) = wellTyped
declTypeCheck (A_Func p (name, info) args body)
    = case getRetType info of
        (Just ty) -> stmtTypeCheck (name, ty) body
        Nothing   -> invalidRetTypeError p name
    where
      getRetType info = case ctype info of
                          (CFun retTy _) -> Just retTy
                          _              -> Nothing

stmtTypeCheck :: (Identifier, CType) -> A_Stmt -> StateEnv ()
stmtTypeCheck (name, retTy) = stmtTypeCheck'
    where
      stmtTypeCheck' :: A_Stmt -> StateEnv ()
      stmtTypeCheck' (A_EmptyStmt)        = wellTyped
      stmtTypeCheck' (A_ExprStmt e)       = exprTypeCheck e >> wellTyped
      stmtTypeCheck' (A_DeclStmt l)       = wellTyped
      stmtTypeCheck' (A_CompoundStmt s)   = mapM_ stmtTypeCheck' s
      stmtTypeCheck' (A_IfStmt p c tr fl) = ifTypeCheck p c tr fl
      stmtTypeCheck' (A_WhileStmt p c bd) = whileTypeCheck p c bd
      stmtTypeCheck' (A_ReturnStmt p e)   = returnTypeCheck p name retTy e
      stmtTypeCheck' (A_RetVoidStmt p)
          = when (retTy /= CVoid) (retTypeError p name CVoid retTy)

ifTypeCheck :: SourcePos -> A_Expr -> A_Stmt -> A_Stmt -> StateEnv ()
ifTypeCheck p cond tr fls
    = do condTy <- exprTypeCheck cond
         if condTy == CInt
         then wellTyped
         else condError p condTy

whileTypeCheck :: SourcePos -> A_Expr -> A_Stmt -> StateEnv ()
whileTypeCheck p cond body
    = do condTy <- exprTypeCheck cond
         if condTy == CInt
         then wellTyped
         else condError p condTy

returnTypeCheck :: SourcePos -> Identifier -> CType -> A_Expr -> StateEnv ()
returnTypeCheck p name retTy e
    = do ty <- exprTypeCheck e
         if retTy == ty
         then wellTyped
         else retTypeError p name retTy ty



exprTypeCheck :: A_Expr -> StateEnv CType
exprTypeCheck (A_AssignExpr p e1 e2)
    = do {
        checkAssignForm p e1;
        ty1 <- exprTypeCheck e1;
        ty2 <- exprTypeCheck e2;
        if ty1 == ty2 then return ty1 else typeDiffError p "=" ty1 ty2; }
exprTypeCheck (A_UnaryPrim p op e)
    = case op of
        "&" -> addrTypeChcek p e
        "*" -> pointerTypeCheck p e
exprTypeCheck (A_BinaryPrim p op e1 e2)
    | op `elem` ["&&", "||", "*", "/"]
        = do {
            ty1 <- exprTypeCheck e1;
            ty2 <- exprTypeCheck e2;
            if ty1 == CInt && ty2 == CInt
            then return CInt
            else binaryTypeError p op ty1 ty2; }
    | op `elem` ["==", "!=", "<", "<=", ">", ">="]
        = do {
            ty1 <- exprTypeCheck e1;
            ty2 <- exprTypeCheck e2;
            if ty1 == ty2
            then return ty1
            else typeDiffError p op ty1 ty2; }
    | op `elem` ["+", "-"]
        = do {
            ty1 <- exprTypeCheck e1;
            ty2 <- exprTypeCheck e2;
            case (ty1, ty2) of
              (CInt, CInt)        -> return CInt
              (CPointer ty, CInt) -> return $ CPointer ty
              (CArray ty _, CInt) -> return $ CPointer ty
              _                   -> invalidCalcError p op ty1 ty2; }
exprTypeCheck (A_ApplyFunc p (name, info) args)
    = do {
        argTypes <- mapM exprTypeCheck args;
        case info of
          (ObjInfo Func (CFun ty parms) _) -> if argTypes == parms
                                              then return ty
                                              else argumentError p name
          _  -> funcReferError p name }
exprTypeCheck (A_MultiExpr es) = liftM last (mapM exprTypeCheck es)
exprTypeCheck (A_Constant n)   = return CInt
exprTypeCheck (A_IdentExpr (name, info))  = return $ ctype info


addrTypeChcek :: SourcePos -> A_Expr -> StateEnv CType
addrTypeChcek p e = do
  checkAssignForm p e
  ty <- exprTypeCheck e
  if ty == CInt then return (CPointer CInt) else unaryError p "&" CInt ty

pointerTypeCheck :: SourcePos -> A_Expr -> StateEnv CType
pointerTypeCheck p e = do
  ty <- exprTypeCheck e
  case ty of
    (CPointer ty') -> return ty'
    _              -> unaryError p "*" (CPointer CInt) ty

{- Utility -}

checkAddressReferForm :: SourcePos -> A_Expr -> StateEnv ()
checkAddressReferForm _ (A_IdentExpr _) = wellTyped
checkAddressReferForm p _ = addrFormError p

checkAssignForm :: SourcePos -> A_Expr -> StateEnv()
checkAssignForm p (A_IdentExpr (name, ObjInfo kind ty _))
    = case (kind, ty) of
        (Var, (CArray _ _))  -> assignError p
        (Var, _)             -> wellTyped
        (Parm, (CArray _ _)) -> assignError p
        (Parm, _)            -> wellTyped
        (Func, _)            -> assignError p
        (FuncProto, _)       -> assignError p
checkAssignForm p (A_UnaryPrim _ "*" _) = wellTyped
checkAssignForm p _ = assignError p

wellTyped :: StateEnv ()
wellTyped = return ()
