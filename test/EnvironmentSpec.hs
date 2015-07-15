module EnvironmentSpec where

import Text.Parsec.Pos
import Test.Hspec
import Control.Monad.State.Strict
import Data.List hiding(find)
import qualified Data.Map as M
import Control.Exception
import Control.Monad.Writer

import AST
import Environment
import ObjInfo
import AnalyzedAST

getEnvEmpty :: StateEnv a -> Env
getEnvEmpty s = M.map sort $ fst (runWriter $ execStateT s M.empty)

runWithEnv :: StateEnv a -> a
runWithEnv s = fst (runWriter (evalStateT s $ M.fromList
                               [(0, [(ObjInfo "a" Var CInt 0),
                                     (ObjInfo "b" Func (CFun CInt [CPointer CInt]) 0)]),
                                (1, [(ObjInfo "c" Parm (CPointer CInt) 1)]),
                                (2, [(ObjInfo "d" Var CInt 2)])]))

u :: SourcePos
u = newPos "test" 0 0

testcase1 :: Program
testcase1 = [(Decl u [(DeclInt, Variable u "a"),
                      (DeclPointer DeclInt, Variable u "b"),
                      (DeclInt, Sequence u "c" 10),
                      (DeclPointer DeclInt, Sequence u "d" 20)])]

testcase2 :: Program
testcase2 = [(FuncPrototype u DeclInt "a" [(DeclInt, "b"), (DeclPointer DeclInt, "c")])]

testcase3 :: Program
testcase3 = [(FuncDef u DeclInt "a" [] (EmptyStmt u))]

spec :: Spec
spec = do
  describe "Environment" $ do
    it "collect global decralations" $ do
      (getEnvEmpty . collectGlobal) testcase1 `shouldBe`
             M.fromList [(0, [(ObjInfo "a" Var CInt 0),
                              (ObjInfo "b" Var (CPointer CInt) 0),
                              (ObjInfo "c" Var (CArray CInt 10) 0),
                              (ObjInfo "d" Var (CArray (CPointer CInt) 20) 0)])]
      (getEnvEmpty . collectGlobal) testcase2 `shouldBe`
             M.fromList [(0, [(ObjInfo "a" FuncProto
                                           (CFun CInt [CInt, CPointer CInt]) 0)])]
      (getEnvEmpty . collectGlobal) testcase3 `shouldBe`
             M.fromList [(0, [(ObjInfo "a" Func (CFun CInt []) 0)])]

    it "find declaration" $ do
      (runWithEnv $ findObjFromJust u 2 "d") `shouldBe` (ObjInfo "d" Var CInt 2)
      (runWithEnv $ findObjFromJust u 2 "c") `shouldBe` (ObjInfo "c" Parm (CPointer CInt) 1)
      (runWithEnv $ findObjFromJust u 1 "b") `shouldBe`
             (ObjInfo "b" Func (CFun CInt [CPointer CInt]) 0)
      evaluate (runWithEnv $ findObjFromJust u 1 "e") `shouldThrow` anyException
