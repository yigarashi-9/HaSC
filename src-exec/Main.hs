
import HaSC.Prim.Parser
import HaSC.Prim.Semantic
import HaSC.Prim.ASTtoIntermed
import HaSC.NASM.GenCode

import Control.Monad
import Control.Exception
import System.IO
import System.Environment
import System.Exit

main :: IO ()
main = catch (do (fileName:_) <- getArgs
                 ast <- parseProgram fileName
                 let (prog, _) = semanticAnalyze ast
                 putStrLn (nasmGenCode . assignRelAddr . astToIntermed $ prog)) err
    where
      err e = do
        hPutStrLn stderr $ show (e :: SomeException)
        exitFailure
