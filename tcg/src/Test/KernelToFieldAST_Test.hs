module Test.KernelToFieldAST_Test where

import Compile.KernelToFieldAST

import Test.Hspec
import Compile.CAST
import Compile.FAST
import Algebra.PolyRings
import Algebra.NTT
import Util.Util

inputVars :: Int -> [String]
inputVars n = ["x["++show i++"]"|i<-[0..n-1]]

ktfa_spec :: Spec
ktfa_spec = do
  describe "compile Kernels to FAST" $ do
    it "compile Gamma to Diagonal to FAST" $ do
      gk <- return (kernelToLO (Gamma 4 0 4 5))
      io_gk <- return (maybeToIO "failed Phi compilation" gk)
      (io_gk >>= (\x -> compileLOToFieldAST 0 x (inputVars 4))) `shouldReturn` (4, ["t0", "t1", "t2", "t3"], [FieldAssignment "t0" (FieldOpExpr (FMultiply (Constant 1) (Variable "x[0]"))), FieldAssignment "t1" (FieldOpExpr (FMultiply (Constant 2) (Variable "x[1]"))), FieldAssignment "t2" (FieldOpExpr (FMultiply (Constant 4) (Variable "x[2]"))), FieldAssignment "t3" (FieldOpExpr (FMultiply (Constant 3) (Variable "x[3]")))])

    it "compile Phi to sqaure to FAST" $ do
      fk <- return (kernelToLO (Phi 4 4 0 4 5))
      io_fk <- return (maybeToIO "failed Phi compilation" fk)
      (io_fk >>= (\x -> compileLOToFieldAST 0 x (inputVars 4))) `shouldReturn` (4,["t0","t1","t2","t3"],[FieldAssignment "t0" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[0]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[1]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[2]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[3]"))) (Constant 0))))))))),FieldAssignment "t1" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[0]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 2) (Variable "x[1]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 4) (Variable "x[2]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 3) (Variable "x[3]"))) (Constant 0))))))))),FieldAssignment "t2" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[0]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 4) (Variable "x[1]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[2]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 4) (Variable "x[3]"))) (Constant 0))))))))),FieldAssignment "t3" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[0]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 3) (Variable "x[1]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 4) (Variable "x[2]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 2) (Variable "x[3]"))) (Constant 0)))))))))])
  
    it "compile Repeat Phi to Parition to FAST" $ do
      rfk <- return (kernelToLO (Kernel_Repeat 4 2 (Phi 2 2 0 4 5)))
      io_rfk <- return (maybeToIO "failed Kernel_Repeat compilation" rfk)
      (io_rfk >>= (\x -> compileLOToFieldAST 0 x (inputVars 4))) `shouldReturn` (4,["t0","t1","t2","t3"],[FieldAssignment "t0" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[0]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[1]"))) (Constant 0))))),FieldAssignment "t1" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[0]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 4) (Variable "x[1]"))) (Constant 0))))),FieldAssignment "t2" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[2]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[3]"))) (Constant 0))))),FieldAssignment "t3" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[2]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 4) (Variable "x[3]"))) (Constant 0)))))])

    it "compile Phi to ITensor to FAST" $ do
      fk <- return (kernelToLO (Phi 4 2 0 4 5))
      io_fk <- return (maybeToIO "failed Phi compilation" fk)
      (io_fk >>= (\x -> compileLOToFieldAST 0 x (inputVars 4))) `shouldReturn` (4,["t0","t2","t1","t3"],[FieldAssignment "t0" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[0]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[2]"))) (Constant 0))))),FieldAssignment "t1" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[0]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 4) (Variable "x[2]"))) (Constant 0))))),FieldAssignment "t2" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[1]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[3]"))) (Constant 0))))),FieldAssignment "t3" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[1]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 4) (Variable "x[3]"))) (Constant 0)))))])

    it "compile Ext Phi to Partition to FAST" $ do
      efk <- return (kernelToLO (Kernel_Extend 4 2 (\i -> Just (Phi 2 2 (2*i) 4 5))))
      io_efk <- return (maybeToIO "failed Kernel_Extend compilation" efk)
      (io_efk >>= (\x -> compileLOToFieldAST 0 x (inputVars 4))) `shouldReturn` (4,["t0","t1","t2","t3"],[FieldAssignment "t0" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[0]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[1]"))) (Constant 0))))),FieldAssignment "t1" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[0]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 4) (Variable "x[1]"))) (Constant 0))))),FieldAssignment "t2" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[2]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 2) (Variable "x[3]"))) (Constant 0))))),FieldAssignment "t3" (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 1) (Variable "x[2]"))) (FieldOpExpr (FAdd (FieldOpExpr (FMultiply (Constant 3) (Variable "x[3]"))) (Constant 0)))))])
