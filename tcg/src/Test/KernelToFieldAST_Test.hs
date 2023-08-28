module Test.KernelToFieldAST_Test where

import Compile.KernelToFieldAST

import Test.Hspec
import Compile.CAST
import Compile.FAST
import Compile.LOClasses
import Algebra.PolyRings
import Algebra.NTT
import Util.Util
import Algebra.BetterAlgebra

inVars :: Int -> [String]
inVars n = ["x["++show i++"]"|i<-[0..n-1]]

outVars :: Int -> [String]
outVars n = ["y["++show i++"]"|i<-[0..n-1]]

ktfa_spec :: Spec
ktfa_spec = do
  describe "compile Kernels to FAST" $ do

    -- 1 9 13 15
    it "compile Gamma to Diagonal to FAST" $ do
      let mpm = init_ModPrimeMemo 17 8
      gk <- return (kernelToLOC mpm (Gamma 4 1 4 8 17))
      io_gk <- return (maybeToIO "failed Gamma compilation" gk)
      (io_gk >>= (\x -> compileLOCToFieldAST (inVars 4,outVars 4) x)) `shouldReturn` [FieldAssignment "y[0]" (FieldOpExpr (FMultiply (Constant 1) (Variable "x[0]"))),FieldAssignment "y[1]" (FieldOpExpr (FMultiply (Constant 9) (Variable "x[1]"))),FieldAssignment "y[2]" (FieldOpExpr (FMultiply (Constant 13) (Variable "x[2]"))),FieldAssignment "y[3]" (FieldOpExpr (FMultiply (Constant 15) (Variable "x[3]")))]

    -- 1 1 1 1
    -- 1 2 4 3
    -- 1 4 1 4
    -- 1 3 4 2
    it "compile Phi to sqaure to FAST" $ do
      let mpm = init_ModPrimeMemo 5 4
      fk <- return (kernelToLOC mpm (Phi 4 4 0 4 5))
      io_fk <- return (maybeToIO "failed Phi compilation" fk)
      (io_fk >>= (\x -> compileLOCToFieldAST (inVars 4,outVars 4) x)) `shouldReturn` [FieldAssignment "y[0]" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[0]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[1]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[2]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[3]"))) (Constant 0))))))))),FieldAssignment "y[1]" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[0]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 2) (Variable "x[1]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 4) (Variable "x[2]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 3) (Variable "x[3]"))) (Constant 0))))))))),FieldAssignment "y[2]" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[0]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 4) (Variable "x[1]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[2]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 4) (Variable "x[3]"))) (Constant 0))))))))),FieldAssignment "y[3]" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[0]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 3) (Variable "x[1]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 4) (Variable "x[2]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 2) (Variable "x[3]"))) (Constant 0)))))))))]

    -- 1 1 _ _
    -- 1 4 _ _
    -- _ _ 1 1
    -- _ _ 1 4
    it "compile Repeat Phi to Parition to FAST" $ do
      let mpm = init_ModPrimeMemo 5 4
      rfk <- return (kernelToLOC mpm (Kernel_Repeat 4 2 (Phi 2 2 0 4 5)))
      io_rfk <- return (maybeToIO "failed Kernel_Repeat compilation" rfk)
      (io_rfk >>= (\x -> compileLOCToFieldAST  (inVars 4,outVars 4) x)) `shouldReturn` [FieldAssignment "y[0]" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[0]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[1]"))) (Constant 0))))),FieldAssignment "y[1]" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[0]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 4) (Variable "x[1]"))) (Constant 0))))),FieldAssignment "y[2]" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[2]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[3]"))) (Constant 0))))),FieldAssignment "y[3]" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[2]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 4) (Variable "x[3]"))) (Constant 0)))))]


    -- 1 _ 1 _
    -- _ 1 _ 1
    -- 1 _ 4 _
    -- _ 1 _ 4
    it "compile Phi to ITensor to FAST" $ do
      let mpm = init_ModPrimeMemo 5 4
      fk <- return (kernelToLOC mpm (Phi 4 2 0 4 5))
      io_fk <- return (maybeToIO "failed Phi compilation" fk)
      (io_fk >>= (\x -> compileLOCToFieldAST (inVars 4,outVars 4) x)) `shouldReturn` [FieldAssignment "y[0]" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[0]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[2]"))) (Constant 0))))),FieldAssignment "y[2]" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[0]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 4) (Variable "x[2]"))) (Constant 0))))),FieldAssignment "y[1]" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[1]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[3]"))) (Constant 0))))),FieldAssignment "y[3]" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[1]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 4) (Variable "x[3]"))) (Constant 0)))))]

    -- 1 1 _ _
    -- 1 4 _ _
    -- _ _ 1 2
    -- _ _ 1 3
    it "compile Ext Phi to Partition to FAST" $ do
      let mpm = init_ModPrimeMemo 5 4
      efk <- return (kernelToLOC mpm (Kernel_Extend 4 2 (\i -> Just (Phi 2 2 (2*i) 4 5))))
      io_efk <- return (maybeToIO "failed Kernel_Extend compilation" efk)
      (io_efk >>= (\x -> compileLOCToFieldAST (inVars 4,outVars 4) x)) `shouldReturn` [FieldAssignment "y[0]" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[0]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[1]"))) (Constant 0))))),FieldAssignment "y[1]" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[0]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 4) (Variable "x[1]"))) (Constant 0))))),FieldAssignment "y[2]" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[2]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 2) (Variable "x[3]"))) (Constant 0))))),FieldAssignment "y[3]" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[2]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 3) (Variable "x[3]"))) (Constant 0)))))]
