module Dirty where
  
data MaybeDirty a = Clean a | Dirty a
    deriving (Eq, Show)

instance Functor MaybeDirty where
	fmap f (Dirty d) = Dirty $ f d
	fmap f (Clean c) = Clean $ f c
	
instance Applicative MaybeDirty where
	pure c = Clean c
	
	Clean c1 <*> Clean c2 = Clean (c1 c2)
	Dirty d <*> Clean c = Dirty (d c)
	Clean c <*> Dirty d = Dirty (c d)
	Dirty d1 <*> Dirty d2 = Dirty (d1 d2)
	
instance Monad MaybeDirty where
  	return = pure
  	
  	Clean x >>= f = f x
  	Dirty x >>= f = f x

helper :: (Ord a, Num a) => a -> a -> MaybeDirty a
helper c1 c2 = if c1 + c2 > 0 then Clean (c1 + c2) else Dirty (c1 + c2)

makeDirty :: MaybeDirty a -> MaybeDirty a
makeDirty (Clean x) = Dirty x
makeDirty (Dirty x) = Dirty x

addDirty :: (Ord a, Num a) => MaybeDirty a -> MaybeDirty a -> MaybeDirty a 
addDirty (Clean c1) ( Clean c2) = helper c1 c2
addDirty (Dirty d) (Clean c) = makeDirty $ helper c d
addDirty (Clean c) (Dirty d) = makeDirty $ helper c c
addDirty (Dirty d1) ( Dirty d2) = makeDirty $ helper d1 d2


prop_ap_test1 = (pure (,) <*> Clean 1 <*> Clean 2)  ==  Clean (1,2)
prop_ap_test2 = (pure (,) <*> Dirty 1 <*> Clean 2)  ==  Dirty (1,2)
prop_ap_test3 = (pure (,) <*> Clean 1 <*> Dirty 2)  ==  Dirty (1,2)
prop_ap_test4 = (pure (,) <*> Dirty 1 <*> Dirty 2)  ==  Dirty (1,2)

prop_addDirty_test1 = addDirty (Dirty 1) (Clean 2) == Dirty 3
prop_addDirty_test2 = addDirty (Clean 1) (Clean 2) == Clean 3
prop_addDirty_test3 = addDirty (addDirty (Clean 1) (Clean (-2))) (Clean 3) == Dirty 2