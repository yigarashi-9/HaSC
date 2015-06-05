{-# LANGUAGE FlexibleInstances #-}
module ParserSpec where

import Text.Parsec
import Text.Parsec.Pos
import Test.Hspec
import Parser
import AST

u :: SourcePos
u = newPos "test" 0 0

v :: String -> Expr
v = IdentExpr u

c :: Integer -> Expr
c = Constant u

exprSample :: Expr
exprSample = BinaryPrim u "||" (v "a")
             (BinaryPrim u "&&" (v "b")
              (ArrayAccess u (v "c") (MultiExpr u [(v "d")])))

stmtSample1 :: Stmt
stmtSample1 = CompoundStmt u
             [DeclStmt u [(CInt, Variable u "a"), (CInt, Sequence u "b" 20)],
              DeclStmt u [(CPointer CInt, Variable u "c")],
              ExprStmt u $ MultiExpr u $ [AssignExpr u (v "a")
                                                       (BinaryPrim u "+" (v "c") (v "d"))]]

stmtSample2 :: Stmt
stmtSample2 = IfStmt u exprSample stmtSample1 (EmptyStmt u)

longTestCase1 :: String
longTestCase1 = concat ["for(i = 0; i < n; i = i + 1)",
                        "if(a || b && c[d]){int a, b[20]; int *c; a = c + d;}"]

longTestCase2 :: String
longTestCase2 = concat ["while(a || b && c[d])",
                        "{int a, b[20]; int *c; a = c+d;}"]

longTestCase3 :: String
longTestCase3 = concat ["void f(int a, int b, int *c, int d){", longTestCase2, "}"]

spec :: Spec
spec = do
  describe "Parser" $ do
    it "Assign Expr" $ do
      parse assignExpr "" "a = 3" `shouldBe`
                (Right $ AssignExpr u (v "a") (c 3))
      parse assignExpr "" "a || b + 3" `shouldBe`
                (Right $ BinaryPrim u "||" (v "a") (BinaryPrim u "+" (v "b") (c 3)))
      parse assignExpr "" "a || b && c[d]" `shouldBe` (Right exprSample)
      parse assignExpr "" "1 + &a * b" `shouldBe`
                (Right $ BinaryPrim u "+" (c 1)
                           (BinaryPrim u "*" (UnaryPrim u "&" (v "a")) (v "b")))
      parse assignExpr "" "1 + &a * b" `shouldBe`
                (Right $ BinaryPrim u "+" (c 1)
                           (BinaryPrim u "*" (UnaryPrim u "&" (v "a")) (v "b")))
      parse assignExpr "" "a = f(b, c*d)" `shouldBe`
                (Right $ AssignExpr u (v "a") $ ApplyFunc u "f"
                                                [(v "b"),
                                                 BinaryPrim u "*" (v "c") (v "d")])
      parse assignExpr "" "a <= -f(b) + *c" `shouldBe`
                (Right $ BinaryPrim u "<=" (v "a") $ BinaryPrim u "+"
                           (UnaryPrim u "-" (ApplyFunc u "f" [(v "b")]))
                           (UnaryPrim u "*" (v "c")))

    it "Expression" $ do
      parse expr "" "a = 3, f(b)" `shouldBe`
                (Right $ MultiExpr u [(AssignExpr u (v "a") (c 3)),
                                      (ApplyFunc u "f" [(v "b")])])

    it "Statement" $ do
      parse stmt "" ";" `shouldBe` (Right $ EmptyStmt u)
      parse stmt "" "a;" `shouldBe` (Right $ ExprStmt u $ MultiExpr u [(v "a")])
      parse stmt "" "{int a, b[20]; int *c; a = c + d;}" `shouldBe` (Right stmtSample1)
      parse stmt "" "if(a) ; else ;" `shouldBe`
                (Right $ IfStmt u (MultiExpr u [(v "a")]) (EmptyStmt u) (EmptyStmt u))
      parse stmt "" "if(a || b && c[d]){int a, b[20]; int *c; a = c + d;}" `shouldBe`
                (Right $ stmtSample2)
      parse stmt "" longTestCase1 `shouldBe`
                (Right $ CompoundStmt u
                           [ExprStmt u (AssignExpr u (v "i") (c 0)),
                            WhileStmt u (BinaryPrim u "<" (v "i") (v "n"))
                                        (CompoundStmt u
                                         [stmtSample2,
                                          ExprStmt u $ AssignExpr u (v "i")
                                                       (BinaryPrim u "+" (v "i") (c 1))])])
      parse stmt "" longTestCase2 `shouldBe`
                (Right $ WhileStmt u exprSample stmtSample1)
      parse stmt "" "return c[2 + a][b || c];" `shouldBe`
                (Right $ ReturnStmt u $
                         ArrayAccess u (ArrayAccess u (v "c")
                                                      (BinaryPrim u "+" (c 2) (v "a")))
                                       (BinaryPrim u "||" (v "b") (v "c")))

    it "Program" $ do
      parse externalDecl "" "int a, *b, c[100];" `shouldBe`
                (Right $ Decl u [(CInt, Variable u "a"),
                                 (CPointer CInt, Variable u "b"),
                                 (CInt, Sequence u "c" 100)])
      parse externalDecl "" "void func(int a, int *b);" `shouldBe`
                (Right $ FuncPrototype u CVoid "func"
                           [(CInt, "a"), (CPointer CInt, "b")])
      parse externalDecl "" longTestCase3 `shouldBe`
                (Right $ FuncDef u CVoid "f" [(CInt, "a"),
                                              (CInt, "b"),
                                              (CPointer CInt, "c"),
                                              (CInt, "d")]
                 [WhileStmt u exprSample stmtSample1])
