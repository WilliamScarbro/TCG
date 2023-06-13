import Algebra.Fourier

df_test_factor = define_morphism (Factor 2) (Base 8 0 8 17)
df_test_label = define_morphism (Label 2) (Base 8 0 8 17)
df_test_norm = define_morphism (Norm) (Base 8 8 16 17)
df_test_define = define_morphism (Define) (Quo 2 2 0 (Base 1 0 4 5))
df_test_pushin = define_morphism (Pushin) (Quo 4 2 0 (Prod 2 2 (\i -> Just (Base 1 i 4 5))))
