{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Test.CAST_Test where

import Test.Hspec
import Compile.CAST

cast_spec :: Spec
cast_spec = do
  describe "translateCToStr" $ do
    it "translates an empty program to an empty string" $
      translateCToStr (CProgram []) `shouldBe` []

    it "translates a program with one assignment statement" $
      translateCToStr (CProgram [CAssignment "x" (Literal (IntLiteral 42))])
        `shouldBe` ["x = 42;"]

  describe "translateStmt" $ do
    it "translates an assignment statement to a string" $
      translateStmt 0 (CAssignment "x" (Literal (IntLiteral 42)))
        `shouldBe` ["x = 42;"]

  describe "translateExpr" $ do
    it "translates an integer literal to a string" $
      translateExpr (Literal (IntLiteral 42)) `shouldBe` "42"

    it "translates a binary expression to a string" $
      translateExpr (CBinaryOp CAdd (Literal (IntLiteral 2)) (Literal (IntLiteral 2)))
        `shouldBe` "(2 + 2)"

    it "translates a unary expression to a string" $
      translateExpr (CUnaryOp CNegate (Literal (IntLiteral 42))) `shouldBe` "-(42)"

  describe "translateBinaryOp" $ do
    it "translates CAdd to a string" $
      translateBinaryOp CAdd `shouldBe` "+"

  describe "translateUnaryOp" $ do
    it "translates CNegate to a string" $
      translateUnaryOp CNegate `shouldBe` "-"

  describe "translateLiteral" $ do
    it "translates an integer literal to a string" $
      translateLiteral (IntLiteral 42) `shouldBe` "42"

--    it "translates a floating point literal to a string" $
--      translateLiteral (FloatLiteral 3.14) `shouldBe` "3.14"
--
--    it "translates a boolean literal to a string" $ do
--      translateLiteral (BoolLiteral True) `shouldBe` "true"
--      translateLiteral (BoolLiteral False) `shouldBe` "false"
--
--    it "translates a character literal to a string" $
--      translateLiteral (CharLiteral 'a') `shouldBe` "'a'"
