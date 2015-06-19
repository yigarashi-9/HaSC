module Semantic where

import qualified Data.Map as M
import Text.Parsec.Pos
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Debug.Trace

import AST
import Environment

semanticAnalyze :: Program -> String
semanticAnalyze prog = show ret
    where ret = evalState (collectGDecl prog >> analyze prog) M.empty

{- 関数の中身を解析していく
   各ブロックのスコープを実現するために環境構築と解析を同時に行う -}
analyze :: Program -> StateEnv String
analyze prog = liftM concat (mapM analyzeFunc prog)


analyzeFunc :: EDecl -> StateEnv String
analyzeFunc (FuncDef p  _ name args stmt)
    = do {
        retTy          <- withEnv 1 (appendWithDupCheck p 1 $ map convParm args)
                          (analyzeStmt 2 stmt);
        (_, CFun ty _) <- find p 0 name;
        if retTy == ty
        then return ""
        else error $ concat [show p, ": return type of '", name, "' does not match"]; }
analyzeFunc _ = return ""


analyzeStmt :: Level -> Stmt -> StateEnv CType
analyzeStmt lev (CompoundStmt _ sts) = withEnv (lev+1) (return ())
                                       (liftM last (mapM (analyzeStmt $ lev+1) sts))
analyzeStmt lev (DeclStmt p l)       = appendWithDupCheck p lev (map convVar l)
                                       >> return CNone
analyzeStmt _   (EmptyStmt _)        = return CNone
analyzeStmt lev (ExprStmt _ e)       = analyzeExpr lev e
analyzeStmt lev s@(IfStmt _ _ _ _)   = analyzeIf   lev s
analyzeStmt lev s@(WhileStmt _ _ _)  = analyzeWhile lev s
analyzeStmt lev (ReturnStmt _ e)     = analyzeExpr lev e
analyzeStmt lev (RetVoidStmt _)      = return CVoid


analyzeIf :: Level -> Stmt -> StateEnv CType
analyzeIf lev (IfStmt p cond tr fls)
    = do {
        ty <- analyzeExpr lev cond;
        if ty == CInt
        then analyzeStmt lev tr >> analyzeStmt lev fls
        else error $ show p ++ ": condifion of if statement must be Int"; }


analyzeWhile :: Level -> Stmt -> StateEnv CType
analyzeWhile lev (WhileStmt p cond body)
    = do {
        ty <- analyzeExpr lev cond;
        if ty == CInt
        then analyzeStmt lev body
        else error $ show p ++ ": condifion of while statement must be Int"; }


analyzeExpr :: Level -> Expr -> StateEnv CType
analyzeExpr lev (AssignExpr p e1 e2)
    = do {
        ty1 <- analyzeExpr lev e1;
        ty2 <- analyzeExpr lev e2;
        if ty1 == ty2
        then return ty1
        else typeDiffError p "="; }
analyzeExpr lev (UnaryPrim p op e)
    = case op of
        "&" -> analyzeUnary p lev op e CInt (CPointer CInt)
        "*" -> analyzeUnary p lev op e (CPointer CInt) CInt
analyzeExpr lev (BinaryPrim p op e1 e2)
    | op `elem` ["&&", "||", "*", "/"]
        = do {
            ty1 <- analyzeExpr lev e1;
            ty2 <- analyzeExpr lev e2;
            if ty1 == CInt && ty2 == CInt
            then return CInt
            else typeSpecError p op CInt; }
    | op `elem` ["==", "!=", "<", "<=", ">", ">="]
        = do {
            ty1 <- analyzeExpr lev e1;
            ty2 <- analyzeExpr lev e2;
            if ty1 == ty2
            then return ty1
            else typeDiffError p op; }
    | op `elem` ["+", "-"]
        = do {
            ty1 <- analyzeExpr lev e1;
            ty2 <- analyzeExpr lev e2;
            case (ty1, ty2) of
              (CInt, CInt)          -> return CInt
              (CPointer CInt, CInt) -> return $ CPointer CInt
              _                     -> error $ show p ++ ": bad pointer operands"; }
analyzeExpr lev (ApplyFunc p nm args)
    = do {
        argTypes <- mapM (analyzeExpr lev) args;
        func     <- find p lev nm;
        case func of
          (Func, CFun ty args) -> if argTypes == args
                                  then return ty
                                  else error $ show p ++ ": bad arguments"
          _                    -> error $ show p ++ ": '" ++ nm ++ "' is not function" }
analyzeExpr lev (MultiExpr p es) = liftM last (mapM (analyzeExpr lev) es)
analyzeExpr lev (Constant p n)   = return CInt
analyzeExpr lev (IdentExpr p n)  = liftM snd (find p lev n)


analyzeUnary :: SourcePos -> Level -> String -> Expr -> CType -> CType
             -> StateEnv CType
analyzeUnary p lev op e srcTy retTy = do
  ty <- analyzeExpr lev e
  if ty == srcTy
  then return retTy
  else typeSpecError p op srcTy


-- 二つの引数の型が同じでなければならない場合の Type Error
typeDiffError :: SourcePos -> String -> a
typeDiffError p op = error $ concat [show p, ": Types of operands '",
                                     op, "' must be same"]

-- 二つの引数の型がある型でなければいけない場合の Type Error
typeSpecError :: SourcePos -> String -> CType -> a
typeSpecError p op ty = error $ concat [show p, ": Types of operands '",
                                       op, "' must be ", show ty]
