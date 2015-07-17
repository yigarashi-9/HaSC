module HaSC.Prim.Environment where

import qualified Data.Map as M
import           Data.List
import           Text.Parsec.Pos
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Writer
import           Control.Monad.State.Strict

import HaSC.Prim.AST
import HaSC.Prim.ObjInfo
import HaSC.Prim.AnalyzedAST
import HaSC.Prim.ErrorMsg


type StateEnv = StateT Env (Writer [String])
type Env      = M.Map Level [ObjInfo]

runEnv :: StateEnv a -> Env -> (a, [String])
runEnv s env = runWriter (evalStateT s env)

initialEnv :: Env
initialEnv = M.fromList [(0, [ObjInfo "print" Func (CFun CVoid [CInt]) 0])]


{- レベルを表す数値 -}
global_lev :: Level
global_lev = 0

param_lev :: Level
param_lev = 1

{- Compound Statement は中でレベルが一つ下がるので
   -1 した値を与えておく -}
func_lev :: Level
func_lev = 1


{- グローバルレベルの情報を集めた状態を返す -}
collectGlobal :: Program -> StateEnv ()
collectGlobal = mapM_ collectEdecl


{- EDeclから情報を抽出する。
   不正な宣言はここで検出する -}
collectEdecl :: EDecl -> StateEnv ()
collectEdecl (Decl p l) = mapM_ (extendEnv p global_lev) (map (makeVarInfo p global_lev) l)
collectEdecl (FuncPrototype p ty nm args)
    = do let funcInfo = makeFuncInfo nm ty args FuncProto
         info <- findAtTheLevel global_lev nm
         case info of
           (Just i) -> if i == funcInfo -- 型情報が同じ宣言は通す
                       then return ()
                       else error $ protoTypeError p nm
           Nothing  -> addEnv global_lev funcInfo
collectEdecl (FuncDef p dcl_ty nm args stmt)
    = do let funcInfo = makeFuncInfo nm dcl_ty args Func
         maybeInfo <- findAtTheLevel global_lev nm
         case maybeInfo of
           (Just info) -> case info of
                            (ObjInfo _ FuncProto ty _)
                                -> if ty == objCtype funcInfo
                                   then addEnv global_lev funcInfo
                                   else error $ funcDeclError p nm
                            (ObjInfo _ Func _ _) -> error $ funcDeclError p nm
                            _                    -> error $ duplicateError p nm
           Nothing    -> addEnv global_lev funcInfo


makeFuncInfo :: Identifier -> DeclType -> [(DeclType, Identifier)] -> Kind -> ObjInfo
makeFuncInfo nm ty args kind = ObjInfo nm kind funTy global_lev
    where funTy = CFun (convType ty) (map (convType . fst) args)

makeVarInfo :: SourcePos -> Level -> (DeclType, DirectDecl) -> ObjInfo
makeVarInfo p lev (dcl_ty, dcl)
    = let ty = convType dcl_ty in
      if containVoid ty
      then error $ voidError p
      else case dcl of
             (Variable _ nm)      -> ObjInfo nm Var ty lev
             (Sequence _ nm size) -> ObjInfo nm Var (CArray ty size) lev

makeParmInfo :: SourcePos -> (DeclType, Identifier) -> ObjInfo
makeParmInfo p (dcl_ty, nm)
    = let ty = convType dcl_ty in
      if containVoid ty
      then error $ voidError p
      else ObjInfo nm Parm ty param_lev


{- 重複を調べずに与えられた info を環境に追加する -}
addEnv :: Level -> ObjInfo -> StateEnv ()
addEnv lev info_entry = do env <- get
                           put $ M.insertWith (++) lev [info_entry] env


{- 重複を調べてから環境を拡張する -}
extendEnv :: SourcePos -> Level -> ObjInfo -> StateEnv ()
extendEnv p lev info
    = do let nm = objName info
         dupInfo <- findAtTheLevel lev nm
         case dupInfo of
           Nothing  -> tellShadowing p nm lev >> addEnv lev info
           (Just _) -> error $ duplicateError p nm


tellShadowing :: SourcePos -> Identifier -> Level -> StateEnv ()
tellShadowing p nm baseLev
    = do hiddenInfo <- findObj (baseLev-1) nm
         case hiddenInfo of
           (Just _) -> tell $ [warningMsg p nm]
           Nothing  -> return ()

deleteLevel :: Level -> StateEnv ()
deleteLevel lev = (liftM (M.delete lev) get) >>= put


withEnv :: Level       -- エントリーポイントのレベル
        -> StateEnv () -- 環境の追加操作
        -> StateEnv a  -- 追加した環境のもとで実行する本体
        -> StateEnv a  -- 上で追加した環境を削除してから本体の結果を返す
withEnv lev mkenv body = mkenv >> body <* deleteLevel lev


findObj :: Level -> Identifier -> StateEnv (Maybe ObjInfo)
findObj lev nm = if lev <= -1
                 then return Nothing
                 else do env <- get;
                         case (M.lookup lev env >>= lookupObj nm) of
                           (Just info) -> return $ Just info
                           Nothing     -> findObj (lev-1) nm

findObjFromJust :: SourcePos -> Level -> Identifier -> StateEnv ObjInfo
findObjFromJust p lev nm
    = do maybeInfo <- findObj lev nm
         case maybeInfo of
           (Just info) -> return info
           Nothing     -> error $ undefinedError p nm

findAtTheLevel :: Level -> Identifier -> StateEnv (Maybe ObjInfo)
findAtTheLevel lev nm = liftM (\e -> M.lookup lev e >>= lookupObj nm) get

lookupObj :: Identifier -> [ObjInfo] -> Maybe ObjInfo
lookupObj nm l = find ((== nm) . objName) l

convType :: DeclType -> CType
convType (DeclPointer ty) = CPointer (convType ty)
convType (DeclInt)        = CInt
convType (DeclVoid)       = CVoid
