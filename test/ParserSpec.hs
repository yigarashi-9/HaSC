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

spec :: Spec
spec = do
  describe "Parser" $ do
    it "Assign Expr" $ do
      parse assignExpr "" "a = 3" `shouldBe`
                (Right $ AssignExpr u (v "a") (c 3))
      parse assignExpr "" "a || b + 3" `shouldBe`
                (Right $ BinaryPrim u "||" (v "a") (BinaryPrim u "+" (v "b") (c 3)))
      parse assignExpr "" "a || b && c[d]" `shouldBe`
                (Right $ BinaryPrim u "||" (v "a")
                           (BinaryPrim u "&&" (v "b")
                              (ArrayAccess u (v "c") (MultiExpr u [(v "d")]))))
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
      parse stmt "" "{int a, b[20]; int *c; a = c;}" `shouldBe`
                (Right $ CompoundStmt u
                         [DeclStmt u [(CInt, Variable u "a"), (CInt, Sequence u "b" 20)],
                          DeclStmt u [(CPointer CInt, Variable u "c")],
                          ExprStmt u $ MultiExpr u $ [AssignExpr u (v "a") (v "c")]])
