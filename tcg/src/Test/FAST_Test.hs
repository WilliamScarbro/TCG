module Test.FAST_Test where

import Test.Hspec
import Compile.CAST
import Compile.FAST

fast_spec :: Spec
fast_spec = do
  describe "getVars" $ do
    it "returns an empty list when given an empty FieldAST" $ do
      getVars (FieldAST []) `shouldBe` []

    it "returns a list of unique variable names used in the FieldAST" $ do
      let fexpr1 = Variable "x"
          fexpr2 = FieldOpExpr (FAdd (Variable "y") (Constant 1))
          stmt1 = FieldAssignment "z" fexpr1
          stmt2 = FieldAssignment "w" fexpr2
          ast = FieldAST [stmt1, stmt2]
      getVars ast `shouldBe` ["x", "z", "y", "w"]

  describe "translateFieldToC" $ do
    it "translates a FieldAST to a CProgram" $ do
      let field = FField 7
          fexpr1 = Variable "x"
          fexpr2 = FieldOpExpr (FMultiply (Variable "y") (Constant 3))
          stmt1 = FieldAssignment "z" fexpr1
          stmt2 = FieldAssignment "w" fexpr2
          ast = FieldAST [stmt1, stmt2]
          expected = CProgram
            [ CAssignment "z" (Identifier "x")
            , CAssignment "w" (CBinaryOp CModulo (CBinaryOp CMultiply (Identifier "y") (Literal (IntLiteral 3))) (Literal (IntLiteral 7)))
            ]
      translateFieldToC field ast `shouldBe` expected

  describe "translateFieldExpr" $ do
    let field = FField 5
    it "translates a Constant FieldExpr to a Literal" $ do
      translateFieldExpr field (Constant 42) `shouldBe` Literal (IntLiteral 42)

    it "translates a Variable FieldExpr to an Identifier" $ do
      translateFieldExpr field (Variable "x") `shouldBe` Identifier "x"

    it "translates a FieldOpExpr to a CExpr" $ do
      let fexpr1 = Variable "x"
          fexpr2 = FieldOpExpr (FAdd (Variable "y") (Constant 1))
          fop = FMultiply fexpr1 fexpr2
          expected = CBinaryOp CModulo (CBinaryOp CMultiply (Identifier "x") (CBinaryOp CModulo (CBinaryOp CAdd (Identifier "y") (Literal (IntLiteral 1))) (Literal (IntLiteral 5)))) (Literal (IntLiteral 5))
          --expected = CBinaryOp CModulo (CBinaryOp CMultiply (Identifier "x") (CBinaryOp CAdd (Identifier "y") (Literal (IntLiteral 1)))) (Literal (IntLiteral 5))
      translateFieldExpr field (FieldOpExpr fop) `shouldBe` expected

  describe "translateFieldStmt" $ do
    let field = FField 11
    it "translates a FieldAssignment to a CAssignment" $ do
      let fexpr = FieldOpExpr (FAdd (Variable "x") (Constant 1))
          stmt = FieldAssignment "y" fexpr
          expected = CAssignment "y" (CBinaryOp CModulo (CBinaryOp CAdd (Identifier "x") (Literal (IntLiteral 1))) (Literal (IntLiteral 11)))
      translateFieldStmt field stmt `shouldBe` expected

  describe "simplify Field Expressions" $ do
    let field = FField 11
    it "removes zeros from an AST" $ do
      let ast = FieldAST [
                  FieldAssignment "x" (Constant 0),
                  FieldAssignment "y" (FieldOpExpr (FAdd (Constant 1) (Constant 0))),
                  FieldAssignment "z" (FieldOpExpr (FSubtract (Constant 2) (Constant 0))),
                  FieldAssignment "w" (FieldOpExpr (FMultiply (Constant 3) (Constant 0))),
                  FieldAssignment "v" (FieldOpExpr (FDivide (Constant 4) (Constant 0))),
                  FieldAssignment "u" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 0) (Variable "x"))) (Constant 0)))
                ]
      let expected_ast = FieldAST [
              FieldAssignment "x" (Constant 0),
              FieldAssignment "y" (Constant 1),
              FieldAssignment "z" (Constant 2),
              FieldAssignment "w" (Constant 0),
              FieldAssignment "v" (FieldOpExpr (FDivide (Constant 4) (Constant 0))),
              FieldAssignment "u" (Constant 0)
            ]
      removeZeros ast `shouldBe` expected_ast

    it "removes ones from an AST" $ do
      let ast = FieldAST [
                  FieldAssignment "x" (Constant 1),
                  FieldAssignment "y" (FieldOpExpr (FAdd (Constant 1) (Constant 1))),
                  FieldAssignment "z" (FieldOpExpr (FSubtract (Constant 2) (Constant 1))),
                  FieldAssignment "w" (FieldOpExpr (FMultiply (Constant 3) (Constant 1))),
                  FieldAssignment "v" (FieldOpExpr (FDivide (Constant 4) (Constant 1))),
                  FieldAssignment "u" (FieldOpExpr (FDivide (FieldOpExpr (FMultiply (Constant 1) (Variable "x"))) (Constant 1)))
                ]
      let expected_ast = FieldAST [
              FieldAssignment "x" (Constant 1),
              FieldAssignment "y" (FieldOpExpr (FAdd (Constant 1) (Constant 1))),
              FieldAssignment "z" (FieldOpExpr (FSubtract (Constant 2) (Constant 1))),
              FieldAssignment "w" (Constant 3),
              FieldAssignment "v" (Constant 4),
              FieldAssignment "u" (Variable "x")
            ]
      removeOnes ast `shouldBe` expected_ast

    it "remove ones from real example t3 = ((((1 * x[1]) % 17) + ((16 * x[9]) % 17)) % 17)" $ do
      let ast = FieldAST [
            FieldAssignment "t3" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[1]"))) (FieldOpExpr (FMultiply (Constant 16) (Variable "x[9]"))))) ] in
        let expected_ast = FieldAST [
              FieldAssignment "t3" (FieldOpExpr (FAdd (Variable "x[1]") (FieldOpExpr (FMultiply (Constant 16) (Variable "x[9]"))))) ] in
          removeOnes ast `shouldBe` expected_ast
