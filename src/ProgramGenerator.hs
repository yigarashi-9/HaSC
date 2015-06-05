module ProgramGenerator where

import Data.List
import AST
import Parser
import Control.Monad

convProgram :: Program -> String
convProgram []     = ""
convProgram (p:ps) = concat [convEDecl p, convProgram ps]

convEDecl :: EDecl -> String
convEDecl (Decl _ dl)                     = convDeclList dl
convEDecl (FuncPrototype _ ty name parms) = convFuncProt ty name parms ++ ";\n"
convEDecl (FuncDef _ ty name parms stmts)
    = convFuncProt ty name parms ++ convStmts stmts


convDeclList :: DeclList -> String
convDeclList = concat . foldr f []
    where f (ty, decl) acc = (concat [show ty, show decl, ";\n"]):acc


convFuncProt :: Type -> Identifier -> [(Type, Identifier)] -> String
convFuncProt ty name parms
    = concat [show ty, name, "(", convParams parms, ")"]


convParams :: [(Type, Identifier)] -> String
convParams = concat . intersperse ", " . foldr (\(ty, i) acc -> (show ty ++ i):acc) []


convStmts :: [Stmt] -> String
convStmts [s]   = convStmt s
convStmts stmts = concat ["{\n",
                          foldr (\s acc -> convStmt s ++ "\n" ++ acc) "" stmts,
                          "}\n"]

convStmt :: Stmt -> String
convStmt (EmptyStmt _)              = ";"
convStmt (ExprStmt _ e)             = convExpr e ++ ";"
convStmt (DeclStmt _ d)             = convDeclList d
convStmt (CompoundStmt _ s)         = convStmts s
convStmt (IfStmt _ cond true false) = concat ["if(", convExpr cond, ") ",
                                              convStmt true, "else ",
                                              convStmt false]
convStmt (WhileStmt _ cond body)    = concat ["while(", convExpr cond, ") ",
                                              convStmt body]
convStmt (ReturnStmt _ ret)         = concat ["return ", convExpr ret]


convExpr (AssignExpr _ dest src)  = convExpr dest ++ " = " ++ convExpr src
convExpr (UnaryPrim _ op e)       = concat [op, "(", convExpr e, ")"]
convExpr (BinaryPrim _ op e1 e2)  = concat [convExpr e1, " ", op, " ", convExpr e2]
convExpr (ArrayAccess _ d s)      = concat [convExpr d, "[", convExpr s, "]"]
convExpr (ApplyFunc _ func parms) = concat [func, " ", convExprs parms]
convExpr (MultiExpr _ es)         = convExprs es
convExpr (Constant _ i)           = show i
convExpr (IdentExpr _ i)          = i


convExprs :: [Expr] -> String
convExprs = concat . intersperse "," . map convExpr
