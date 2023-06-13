import Algebra.FField
import Algebra.NTT


test_ntt1 = (mNTT 4 5) `mv` (ffVec 4 5 id)
test_ntt2 = (mNTT 4 5) `mm` (mNTT 4 5) >>= (mm (mNTT 4 5))
test_ntt3 = ((mNTT 4 5) `mm` (mNTT_inv 4 5)) == Just (mId 4)

test_tensor1 = tensor (mId 2) (mNTT 3 7)
test_tensor2 = tensor (mNTT 3 7) (mId 2)
test_tensor3 = (mL 6 2) * (tensor (mNTT 3 7) (mId 2)) * (mL 6 3)
test_tensor4 = test_tensor1 == test_tensor3

test_phi1_1 = phi 6 3 0 6 7
--test_phi1_2 = (mL 6 2) * (phi 3 3 0 6 7) * (mL 6 3)
--test_phi1_3 = test_phi1_1 == test_phi1_2
--
test_phi2_1 = (mL 6 2) * (phi 6 3 6 12 13) * (mL 6 3)
test_phi2_2 = tensor (mId 2) (phi 3 3 6 12 13) 
test_phi2_3 = test_phi2_1 == test_phi2_2

-- for some reason we have to transpose to get an inverse, this sort of matches the gxg^-1 group action
test_phi3 = (phi 6 3 6 12 13) * (tpose (phi_inv 6 3 6 12 13)) == (mId 6)


test_gamma1 = gamma 3 3 6 7
test_gamma2 = gamma_inv 3 3 6 7
test_gamma3 = test_gamma1 * test_gamma2 == mId 3



test_repeatLO = repeatLO 2 (phi 2 2 0 8 17)

test_extend_0 = phi 2 2 0 8 17
test_extend_1 = phi 2 2 4 8 17
test_extendLO = extendLO 2 2 (\x -> phi 2 2 (4*x) 8 17)


-- L_m 
--test_spiral = 
