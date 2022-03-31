module Week4a where


safeSquareRoot :: Double -> Maybe Double
safeSquareRoot x = if x < 0 then Nothing else Just (sqrt x)

multiplyFmap x y = fmap (*2) (safeSquareRoot (x * y))
-- if x > 0 && y > 0 then Just $ x * y * 2 else Nothing

multiplySqrtDouble :: Double -> Double -> Maybe Double
multiplySqrtDouble x y = case safeSquareRoot product of
  	Nothing -> Nothing
  	Just z  -> Just (z * 2)
  	where
    	product = x * y


data Metrics m = Metrics
  { latestMeasurements :: [m]
  , average :: m
  , max :: m
  , min :: m
  , mode :: Maybe m
  } deriving (Show, Eq)

{- First define your `Functor` instance here -}

instance Functor Metrics where
  	fmap f (Metrics a b c d e) = Metrics (fmap f a) (f b) (f c) (f d) (fmap f e)

doubleMetrics :: Metrics Double -> Metrics Double
doubleMetrics = fmap (*2)



data Tree a = Leaf | Node (Tree a) a (Tree a)
  	deriving (Show, Eq)
  
instance Functor Tree where
  	fmap _ Leaf = Leaf
  	fmap f (Node left root right) = Node (fmap f left) (f root) (fmap f right)
  	
  	
  	
  	
  	
data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
  	deriving (Show, Eq)
  
instance Functor Expr where
  	-- fmap :: (a -> b) -> Expr a -> Expr b
  	fmap f (Var x) = Var $ f x
  	fmap _ (Val x) = Val x
  	fmap f (Add left right) = Add (fmap f left) (fmap f right)




generateAllResults :: [Int -> Int -> Int] -> [Int] -> [Int] -> [Int]
generateAllResults fs xs ys = fs <*> xs <*> ys





newtype ZipList a = Z [a]
  	deriving (Show, Eq)
  
instance Functor ZipList where
  	-- fmap :: (a -> b) -> ZipList a -> ZipList b
  	fmap g (Z xs) = Z (fmap g xs)
  
instance Applicative ZipList where
  	-- pure :: a -> ZipList a
  	pure x = Z (repeat x)
  
  	-- (<*>) :: ZipList (a -> b) -> ZipLis	t a -> ZipList b
  	(Z gs) <*> (Z xs) = Z (zipWith ($) gs xs)
