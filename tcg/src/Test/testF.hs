

data MyMaybe a = MyJust a | MyNothing

instance Functor (MyMaybe) where
  fmap f (MyJust a) = MyJust (f a)
  fmap f MyNothing = MyNothing

data MetaFunctor = Meta Functor 
