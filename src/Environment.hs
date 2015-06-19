module Environment where

import qualified Data.Map as M
import           Text.Parsec.Pos
import           Control.Applicative
import           Control.Monad
import           Control.Monad.State

import AST

type Env   = M.Map Level [Info]
type Level = Int
type Info  = (Identifier, (Kind, CType))
data Kind  = Var | Func | Parm deriving(Show, Eq, Ord)
data CType = CInt
           | CVoid
           | CNone
           | CPointer CType
           | CArray   CType Integer
           | CFun     CType [CType]
           deriving(Show, Eq, Ord)


-- グローバルな宣言だけを集める
collectGDecl :: Program -> State Env ()
collectGDecl = mapM_ collectEdecl

collectEdecl :: EDecl -> State Env ()
collectEdecl (Decl _ l)                   = mapM_ (appendEnv 0) (map convVar l)
collectEdecl (FuncPrototype _ ty nm args) = appendEnv 0 (funcDecl nm ty args)
collectEdecl (FuncDef  _ ty nm args stmt) = appendEnv 0 (funcDecl nm ty args)

funcDecl :: Identifier -> DeclType -> [(DeclType, Identifier)] -> Info
funcDecl nm ty args = (nm, (Func, CFun (convType ty) $ map (convType . fst) args))

convVar :: (DeclType, DirectDecl) -> Info
convVar (t, d) = let ty = convType t in
                 case d of
                   (Variable _ n)    -> (n, (Var, ty))
                   (Sequence _ n sz) -> (n, (Var, CArray ty sz))

convParm :: (DeclType, Identifier) -> Info
convParm (t, n) = (n, (Var, convType t))

convType :: DeclType -> CType
convType (DeclPointer ty) = CPointer (convType ty)
convType (DeclInt)        = CInt
convType (DeclVoid)       = CVoid

appendEnv :: Level -> Info -> State Env ()
appendEnv lev info = liftM (M.insertWith (++) lev [info]) get >>= put

deleteLevel :: Level -> State Env ()
deleteLevel lev = (liftM (M.delete lev) get) >>= put

withEnv :: Level -> [Info] -> State Env a -> State Env a
withEnv lev l body = mapM_ (appendEnv lev) l >> body <* deleteLevel lev

find :: SourcePos -> Level -> Identifier -> State Env (Kind, CType)
find p lev name = do
  if lev <= (-1)
  then error $ concat [show p, ": varialble '", name, "' is not defined."]
  else do
    env <- (get :: State Env Env)
    case (M.lookup lev env >>= lookup name) of
      (Just info) -> return info
      Nothing     -> find p (lev-1) name
