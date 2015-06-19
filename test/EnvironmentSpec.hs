module EnvironmentSpec where

import Text.Parsec.Pos
import Test.Hspec
import Control.Monad.State
import Data.List hiding(find)
import qualified Data.Map as M
import Control.Exception

import AST
import Environment

getEnvEmpty :: StateEnv a -> Env
getEnvEmpty s = M.map sort $ execState s M.empty

runWithEnv :: StateEnv a -> a
runWithEnv s = evalState s $ M.fromList [(0, [("a", (Var, CInt)),
                                              ("b", (Func, CFun CInt [CPointer CInt]))]),
                                         (1, [("c", (Parm, CPointer CInt))]),
                                         (2, [("d", (Var, CInt))])]

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
      (getEnvEmpty . collectGDecl) testcase1 `shouldBe`
             M.fromList [(0, [("a", (Var, CInt)),
                              ("b", (Var, CPointer CInt)),
                              ("c", (Var, CArray CInt 10)),
                              ("d", (Var, CArray (CPointer CInt) 20))])]
      (getEnvEmpty . collectGDecl) testcase2 `shouldBe`
             M.fromList [(0, [("a", (Func, CFun CInt [CInt, CPointer CInt]))])]
      (getEnvEmpty . collectGDecl) testcase3 `shouldBe`
             M.fromList [(0, [("a", (Func, CFun CInt []))])]

    it "find declaration" $ do
      (runWithEnv $ find u 2 "d") `shouldBe` (Var, CInt)
      (runWithEnv $ find u 2 "c") `shouldBe` (Parm, CPointer CInt)
      (runWithEnv $ find u 1 "b") `shouldBe` (Func, CFun CInt [CPointer CInt])
      evaluate (runWithEnv $ find u 1 "e") `shouldThrow` anyException
