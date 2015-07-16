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
semanticAnalyze prog = runEnv body initialEnv
    where body = do collectGlobal prog
                    ret <- analyze prog
                    case typeCheck ret of
                      (Left errMsg) -> error errMsg
                      (Right _)     -> return ret

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
                          (mapM_ (addEnv param_lev) parms)
                          (analyzeStmt func_lev stmt);
        func   <- findFromJust p global_lev name;
        if name == "main" && ctype func == CFun CInt []
        then return $ [A_Func p (name, func) parms (addReturn a_stmt)]
        else return $ [A_Func p (name, func) parms a_stmt]; }

addReturn :: A_Stmt -> A_Stmt
addReturn (A_CompoundStmt stmts) = A_CompoundStmt (stmts ++
                                                   [A_ReturnStmt p (A_Constant 0)])
    where p = newPos "hoge" 0 0
addReturn _ = error "never happen"

analyzeStmt :: Level -> Stmt -> StateEnv A_Stmt
analyzeStmt lev (CompoundStmt _ stmts)
    = withEnv (lev+1)
              (return ())
              (liftM A_CompoundStmt (mapM (analyzeStmt $ lev+1) stmts))
analyzeStmt lev (DeclStmt p dcls)
    = let info = map (makeVarInfo p lev) dcls
      in mapM_ (addEnv lev) info >> (return $ A_DeclStmt info)
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
              -> error $ funcReferError p name
          (ObjInfo FuncProto _ _)
              -> error $ funcReferError p name
          validInfo -> return $ A_IdentExpr (name, validInfo); }


{- typeCheck -}
typeCheck :: A_Program -> Check ()
typeCheck = mapM_ declTypeCheck

declTypeCheck :: A_EDecl -> Check ()
declTypeCheck (A_Decl _) = return ()
declTypeCheck (A_Func p (name, info) args body)
    = case getRetType info of
        (Just ty) -> do retTy <- stmtTypeCheck (name, ty) body
                        when (ty /= retTy) (fail $ invalidRetTypeError p name)
        Nothing   -> fail $ invalidRetTypeError p name
    where
      getRetType info = case ctype info of
                          (CFun retTy _) -> Just retTy
                          _              -> Nothing

stmtTypeCheck :: (Identifier, CType) -> A_Stmt -> Check CType
stmtTypeCheck info@(name, retTy) = stmtTypeCheck'
    where
      stmtTypeCheck' :: A_Stmt -> Check CType
      stmtTypeCheck' (A_EmptyStmt)        = wellTyped
      stmtTypeCheck' (A_ExprStmt e)       = exprTypeCheck e >> wellTyped
      stmtTypeCheck' (A_DeclStmt l)       = wellTyped
      stmtTypeCheck' (A_CompoundStmt s)   = foldCompoundStmt info CVoid s
      stmtTypeCheck' (A_IfStmt p c tr fl) = ifTypeCheck p info c tr fl
      stmtTypeCheck' (A_WhileStmt p c bd) = whileTypeCheck p info c bd
      stmtTypeCheck' (A_ReturnStmt p e)   = returnTypeCheck p info e
      stmtTypeCheck' (A_RetVoidStmt p)
          = if retTy /= CVoid
            then fail $ retTypeError p name retTy CVoid
            else wellTyped

foldCompoundStmt :: (Identifier, CType) -> CType -> [A_Stmt] -> Check CType
foldCompoundStmt info = foldM f
    where
      f acc stmt = do stmtTy <- stmtTypeCheck info stmt
                      return $ synType stmtTy acc

ifTypeCheck :: SourcePos -> (Identifier, CType) -> A_Expr -> A_Stmt -> A_Stmt
            -> Check CType
ifTypeCheck p info cond tr fls
    = do condTy <- exprTypeCheck cond
         if condTy == CInt
         then liftM2 synType (stmtTypeCheck info tr) (stmtTypeCheck info fls)
         else fail $ condError p condTy

whileTypeCheck :: SourcePos -> (Identifier, CType) -> A_Expr -> A_Stmt
               -> Check CType
whileTypeCheck p info cond body
    = do condTy <- exprTypeCheck cond
         if condTy == CInt
         then stmtTypeCheck info body
         else fail $ condError p condTy

returnTypeCheck :: SourcePos -> (Identifier, CType) -> A_Expr -> Check CType
returnTypeCheck p (name, retTy) e
    = do ty <- exprTypeCheck e
         if retTy == ty
         then return ty
         else fail $ retTypeError p name retTy ty


exprTypeCheck :: A_Expr -> Check CType
exprTypeCheck (A_AssignExpr p e1 e2)
    = do {
        checkAssignForm p e1;
        ty1 <- exprTypeCheck e1;
        ty2 <- exprTypeCheck e2;
        if ty1 == ty2 then return ty1 else fail $ typeDiffError p "=" ty1 ty2; }
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
            else fail $ binaryTypeError p op ty1 ty2; }
    | op `elem` ["==", "!=", "<", "<=", ">", ">="]
        = do {
            ty1 <- exprTypeCheck e1;
            ty2 <- exprTypeCheck e2;
            if ty1 == ty2
            then return CInt
            else fail $ typeDiffError p op ty1 ty2; }
    | op `elem` ["+", "-"]
        = do {
            ty1 <- exprTypeCheck e1;
            ty2 <- exprTypeCheck e2;
            case (ty1, ty2) of
              (CInt, CInt)        -> return CInt
              (CPointer ty, CInt) -> return $ CPointer ty
              (CArray ty _, CInt) -> return $ CPointer ty
              _                   -> fail $ invalidCalcError p op ty1 ty2; }
exprTypeCheck (A_ApplyFunc p (name, info) args)
    = do {
        argTypes <- mapM exprTypeCheck args;
        case info of
          (ObjInfo Func (CFun ty parms) _) -> if argTypes == parms
                                              then return ty
                                              else fail $ argumentError p name
          _  -> fail $ funcReferError p name }
exprTypeCheck (A_MultiExpr es) = liftM last (mapM exprTypeCheck es)
exprTypeCheck (A_Constant n)   = return CInt
exprTypeCheck (A_IdentExpr (name, info))  = return $ ctype info


addrTypeChcek :: SourcePos -> A_Expr -> Check CType
addrTypeChcek p e = do
  checkAddressReferForm p e
  ty <- exprTypeCheck e
  if ty == CInt then return (CPointer CInt) else fail $ unaryError p "&" CInt ty

pointerTypeCheck :: SourcePos -> A_Expr -> Check CType
pointerTypeCheck p e = do
  ty <- exprTypeCheck e
  case ty of
    (CPointer ty') -> return ty'
    _              -> fail $ unaryError p "*" (CPointer CInt) ty

{- Utility -}

checkAddressReferForm :: SourcePos -> A_Expr -> Check ()
checkAddressReferForm _ (A_IdentExpr _) = return ()
checkAddressReferForm p _ = fail $ addrFormError p

checkAssignForm :: SourcePos -> A_Expr -> Check ()
checkAssignForm p (A_IdentExpr (name, ObjInfo kind ty _))
    = case (kind, ty) of
        (Var, (CArray _ _))  -> fail $ assignError p
        (Var, _)             -> return ()
        (Parm, (CArray _ _)) -> fail $ assignError p
        (Parm, _)            -> return ()
        (Func, _)            -> fail $ assignError p
        (FuncProto, _)       -> fail $ assignError p
checkAssignForm p (A_UnaryPrim _ "*" _) = return ()
checkAssignForm p _ = fail $ assignError p

wellTyped :: Check CType
wellTyped = return CVoid

isPointer :: A_Expr -> Bool
isPointer e = case exprTypeCheck e of
                (Left _)   -> undefined
                (Right ty) -> case ty of
                                (CPointer _) -> True
                                (CArray _ _) -> True
                                _            -> False
