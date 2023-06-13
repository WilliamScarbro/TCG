import Algebra.NTT
import Compile.CompileKernel



comp_test1 = compilePath "Phi" [Phi 4 2 0 4 5]
comp_test2 = compilePath "Rep" [Kernel_Repeat 4 2 (Phi 2 2 0 4 5)]
comp_test3 = compilePath "Ext" [Kernel_Extend 4 2 (\i -> Just (Phi 2 2 (2*i) 4 5))]
comp_test4 = compilePath "Swap" [Phi 4 2 0 4 5, Phi 4 2 0 4 5]

main = do
  putStr comp_test1
  putStr comp_test2
  putStr comp_test3
  putStr comp_test4
  

