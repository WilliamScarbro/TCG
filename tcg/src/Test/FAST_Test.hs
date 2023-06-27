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


----
    it "should add negation to the multiplication expressions" $ do
      let field = FField 3
          fast = FieldAST [FieldAssignment "x" (FieldOpExpr (FMultiply (Constant 2) (Variable "y")))]
          expectedAST = FieldAST [FieldAssignment "x" (FieldOpExpr (FNeg (Variable "y")))] 
      addNegation field fast `shouldBe` expectedAST

    it "should add negation to the multiplication expressions (reversed)" $ do
      let field = FField 5
          fast = FieldAST [FieldAssignment "x" (FieldOpExpr (FMultiply (Variable "y") (Constant 4)))]
          expectedAST = FieldAST [FieldAssignment "x" (FieldOpExpr (FNeg (Variable "y")))]
      addNegation field fast `shouldBe` expectedAST

--    
    it "should remove negation from the addition expr (FT)" $ do
      let fast = FieldAST [FieldAssignment "x" (FieldOpExpr (FAdd (Variable "y") (FieldOpExpr (FNeg (Variable "z")))))]
          expectedAST = FieldAST [FieldAssignment "x" (FieldOpExpr (FSubtract (Variable "y") (Variable "z")))]
      removeNegation fast `shouldBe` expectedAST
    
    it "should remove negation from the addition expr (TF)" $ do
      let fast = FieldAST [FieldAssignment "x" (FieldOpExpr (FAdd (FieldOpExpr (FNeg (Variable "z"))) (Variable "y") ))]
          expectedAST = FieldAST [FieldAssignment "x" (FieldOpExpr (FSubtract (Variable "y") (Variable "z")))]
      removeNegation fast `shouldBe` expectedAST

    it "should remove negation from the addition expr (TT)" $ do
      let fast = FieldAST [FieldAssignment "x" (FieldOpExpr (FAdd (FieldOpExpr (FNeg (Variable "z"))) (FieldOpExpr (FNeg (Variable "y"))) ))]
          expectedAST = FieldAST [FieldAssignment "x" (FieldOpExpr (FNeg (FieldOpExpr (FAdd (Variable "z") (Variable "y")))))]
      removeNegation fast `shouldBe` expectedAST

--
    it "should remove negation from the subtraction expr (FT)" $ do
      let fast = FieldAST [FieldAssignment "x" (FieldOpExpr (FSubtract (Variable "y") (FieldOpExpr (FNeg (Variable "z")))))]
          expectedAST = FieldAST [FieldAssignment "x" (FieldOpExpr (FAdd (Variable "y") (Variable "z")))]
      removeNegation fast `shouldBe` expectedAST

    it "should remove negation from the subtraction expr (TF)" $ do
      let fast = FieldAST [FieldAssignment "x" (FieldOpExpr (FSubtract (FieldOpExpr (FNeg (Variable "z"))) (Variable "y") ))]
          expectedAST = FieldAST [FieldAssignment "x" (FieldOpExpr (FNeg (FieldOpExpr (FAdd (Variable "z") (Variable "y")))))]
      removeNegation fast `shouldBe` expectedAST

    it "should remove negation from the subtraction expr (TT)" $ do
      let fast = FieldAST [FieldAssignment "x" (FieldOpExpr (FSubtract (FieldOpExpr (FNeg (Variable "z"))) (FieldOpExpr (FNeg (Variable "y"))) ))]
          expectedAST = FieldAST [FieldAssignment "x" (FieldOpExpr (FSubtract (Variable "y") (Variable "z")))]
      removeNegation fast `shouldBe` expectedAST

--
  it "real example for add negation" $ do
    let field = FField 5
        fast = FieldAST [FieldAssignment "t0" (FieldOpExpr (FAdd (Variable "X[0]") (Variable "X[2]"))),
                         FieldAssignment "t1" (FieldOpExpr (FAdd (Variable "X[0]") (FieldOpExpr (FMultiply (Constant 4) (Variable "X[2]"))))),
                         FieldAssignment "t2" (FieldOpExpr (FAdd (Variable "X[1]") (Variable "X[3]"))),
                         FieldAssignment "t3" (FieldOpExpr (FAdd (Variable "X[1]") (FieldOpExpr (FMultiply (Constant 4) (Variable "X[3]"))))),
                         FieldAssignment "t4" (FieldOpExpr (FAdd (Variable "t0") (Variable "t2"))),
                         FieldAssignment "t5" (FieldOpExpr (FAdd (Variable "t0") (FieldOpExpr (FMultiply (Constant 4) (Variable "t2"))))),
                         FieldAssignment "t6" (FieldOpExpr (FAdd (Variable "t1") (FieldOpExpr (FMultiply (Constant 2) (Variable "t3"))))),
                         FieldAssignment "t7" (FieldOpExpr (FAdd (Variable "t1") (FieldOpExpr (FMultiply (Constant 3) (Variable "t3")))))]
        expectedAST = FieldAST [FieldAssignment "t0" (FieldOpExpr (FAdd (Variable "X[0]") (Variable "X[2]"))),
                                FieldAssignment "t1" (FieldOpExpr (FAdd (Variable "X[0]") (FieldOpExpr (FNeg (Variable "X[2]"))))),
                                FieldAssignment "t2" (FieldOpExpr (FAdd (Variable "X[1]") (Variable "X[3]"))),
                                FieldAssignment "t3" (FieldOpExpr (FAdd (Variable "X[1]") (FieldOpExpr (FNeg (Variable "X[3]"))))),
                                FieldAssignment "t4" (FieldOpExpr (FAdd (Variable "t0") (Variable "t2"))),
                                FieldAssignment "t5" (FieldOpExpr (FAdd (Variable "t0") (FieldOpExpr (FNeg (Variable "t2"))))),
                                FieldAssignment "t6" (FieldOpExpr (FAdd (Variable "t1") (FieldOpExpr (FMultiply (Constant 2) (Variable "t3"))))),
                                FieldAssignment "t7" (FieldOpExpr (FAdd (Variable "t1") (FieldOpExpr (FMultiply (Constant 3) (Variable "t3")))))]
    addNegation field fast `shouldBe` expectedAST
