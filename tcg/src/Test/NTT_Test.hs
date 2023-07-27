module Test.NTT_Test where

import Algebra.FField
import Algebra.NTT
import Algebra.Fourier
import Algebra.PolyRings

import Test.Hspec

ntt_spec :: Spec
ntt_spec = do
  describe "NTT - linear op defintions of transformations" $ do
    it "NTT 4 5 applied to connon vec 4 5" $ 
      let test_ntt1 = (mNTT 4 5) `mv` (ffVec 4 5 id) in
      test_ntt1 `shouldBe` Just (ffVec 4 5 (\i -> [1,4,3,2]!!i))
    it "NTT by NTT_inv produces identity" $
      let test_ntt2 = ((mNTT 4 5) `mm` (mNTT_inv 4 5)) in
      test_ntt2 `shouldBe` Just (mId 4)
    it "mL correctly reverses tensor product (L n k) (A_m tensor B_k) (L n m) == (B_k tensor A_m)" $
      let test_tensor1 = tensor (mId 2) (mNTT 3 7)
          test_tensor2 = (mL 6 2) * (tensor (mNTT 3 7) (mId 2)) * (mL 6 3) in
      test_tensor1 `shouldBe` test_tensor2
    it "incomplete phi is permutation of tensor of complete phi: (L n m) (phi n k) (L n k) == (I_2 tensor (Phi k k))" $
      let test_phi1_1 = (mL 6 2) * (phi 6 3 6 12 13) * (mL 6 3)
          test_phi1_2 = tensor (mId 2) (phi 3 3 6 12 13) in
      test_phi1_1 `shouldBe` test_phi1_2 
    it "phi by phi_inv produces identity, case 1" $
      let test_phi2_1 = (phi 6 3 6 12 13)
          test_phi2_2 = (phi_inv 6 3 6 12 13)
          test_phi2_3 = test_phi2_1 * (test_phi2_2) in
      -- for some reason we have to transpose to get an inverse, this sort of matches the gxg^-1 group action
      test_phi2_3 `shouldBe` (mId 6)
    it "phi by phi_inv produces identity, case 2" $
      let test_phi2_1 = (phi 16 4 16 32 257)
          test_phi2_2 = (phi_inv 16 4 16 32 257)
          test_phi2_3 = test_phi2_1 * (test_phi2_2) 
      in
        -- for some reason we have to transpose to get an inverse, this sort of matches the gxg^-1 group action
        test_phi2_3 `shouldBe` (mId 16)
    it "phi by phi_inv produces identity, case 3" $
      let test_phi2_1 = (phi 4 4 4 8 17)
          test_phi2_2 = (phi_inv 4 4 4 8 17)
          test_phi2_3 = test_phi2_1 * (test_phi2_2)
      in
        -- for some reason we have to transpose to get an inverse, this sort of matches the gxg^-1 group action
        test_phi2_3 `shouldBe` (mId 4)

    it "gamma by gamma inv produces identity" $
      let test_gamma1 = gamma 3 1 3 6 7
          test_gamma2 = gamma_inv 3 1 3 6 7 in
      test_gamma1 * test_gamma2 `shouldBe` mId 3
        


test_T_1 = mT 2 2 2 -- 8
test_T_2 = mT 3 2 2 -- 12
test_T_3 = mT 2 3 2 -- 12
test_T_4 = mT 2 2 3 -- 12


test_repeatLO = repeatLO 2 (phi 2 2 0 8 17)

-- extend cannot be easily tested since we don't have identities (these should at least not produce errors in construction)

test_extend_0 = phi 2 2 0 8 17
test_extend_1 = phi 2 2 4 8 17
test_extendLO = extendLO 2 2 (\x -> Just (phi 2 2 (4*x) 8 17))

test_extend2 =
  do ring <- apply (Factor 2) (Base 8 0 8 17)
     define_morphism (Extend 2 (Factor 2)) ring



-- L_m 
--test_spiral = 
