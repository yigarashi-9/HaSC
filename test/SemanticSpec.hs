module SemanticSpec where

import Text.Parsec.Pos
import Test.Hspec
import Data.List hiding(find)
import qualified Data.Map as M
import Control.Monad.State.Strict
import Control.Exception

import AST
import Parser
import Environment
import AnalyzedAST
import Semantic

u :: SourcePos
u = newPos "test" 0 0

run :: String -> IO A_Program
run test = do
  ast <- parseProgram ("SemanticTest/" ++ test)
  return $ fst (runEnv (analyze ast))

spec :: Spec
spec = do
  describe "Environment" $ do
    it "detect duplicate decralations" $ do
      run "test_0.c" `shouldThrow` anyException
      run "test_1.c" `shouldThrow` anyException
      run "test_2.c" `shouldThrow` anyException
      run "test_3.c" `shouldThrow` anyException

    it "detect invalid reference" $ do
      run "test_4.c" `shouldThrow` anyException
      run "test_5.c" `shouldThrow` anyException

    it "detect type error" $ do
      run "test_6.c" `shouldThrow` anyException
      run "test_7.c" `shouldThrow` anyException
      run "test_8.c" `shouldThrow` anyException
      run "test_9.c" `shouldThrow` anyException
      run "test_10.c" `shouldThrow` anyException
      run "test_11.c" `shouldThrow` anyException
      run "test_12.c" `shouldThrow` anyException
      run "test_13.c" `shouldThrow` anyException
      run "test_14.c" `shouldThrow` anyException
      run "test_15.c" `shouldThrow` anyException
      run "test_16.c" `shouldThrow` anyException
