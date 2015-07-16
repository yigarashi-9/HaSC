
import Parser
import Semantic
import ASTtoIntermed

import Control.Monad
import Control.Exception
import System.IO
import System.Exit

main :: IO ()
main = catch (do ast <- parseProgram "test/sort.c"
                 let (prog, _) = semanticAnalyze ast
                 withFile "test/test_code.s" WriteMode $ \handle ->
                     hPutStrLn handle (show . astToIntermed $ prog)) err
    where
      err e = do
        hPutStrLn stderr $ show (e :: SomeException)
        exitFailure
