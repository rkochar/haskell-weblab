module Week4b where

import Data.Monoid
import Prelude hiding (sequence, filterM)


safeSquareRoot :: Double -> Maybe Double
safeSquareRoot x = if x < 0 then Nothing else Just (sqrt x)

multiplyIfSmall :: Double -> Double -> Maybe Double
multiplyIfSmall y x = if x < 9.5 then Just (y * x) else Nothing

sqrtAndMultiply :: Double -> Maybe Double
sqrtAndMultiply n = safeSquareRoot n >>= multiplyIfSmall 10



-- xs >>= f = concat (map f xs)
addNegateHelper x = [x + 1, -x - 1, x + 2, -x - 2, x + 3, - x - 3]

addAndNegate :: [Int] -> [Int]
addAndNegate xs = xs >>= addNegateHelper


sumOfSquareRoots :: Double -> Double -> Maybe Double
sumOfSquareRoots x y = do
  	x1 <- safeSquareRoot x
  	y1 <- safeSquareRoot y
  	return (x1 + y1)

generateAllResults :: [Int -> Int -> Int] -> [Int] -> [Int] -> [Int]
generateAllResults fs xs ys = do
  	f <- fs
  	x <- xs
  	y <- ys
  	return (f x y)

sqrtAndMultiplyDo :: Double -> Maybe Double
sqrtAndMultiplyDo x = do
  	x1 <- safeSquareRoot x
  	x2 <- multiplyIfSmall x1 10
  	return x2

addAndNegateDo :: [Int] -> [Int]
addAndNegateDo xs = do
  	x <- xs
  	y <- [x + 1, x + 2, x + 3]
  	z <- [y, -y]
  	return z




sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (m : ms) = do
	x <- m
	xs <- sequence ms
	return $ x : xs



filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = return []
filterM f (m : ms) = do
  	x <- f m
  	xs <- filterM f ms
  	return $ if x then m : xs else xs


while :: Monad m => (m Bool) -> m () -> m ()
while loopCond loopBody = do
  	b <- loopCond
  	if b then do 
  	  loopBody
  	  while loopCond loopBody 
  	else return ()



data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
 	 deriving (Show)

instance Functor Expr where
	fmap f (Var a) = Var (f a)
	fmap _ (Val i) = Val i
	fmap f (Add e1 e2) = Add (fmap f e1) (fmap f e2)

instance Applicative Expr where
	-- pure :: a -> Expr a
	pure x = Var x 
	
	-- (<*>) :: Expr (a -> b) -> Expr a -> Expr b
	Var f <*> x = fmap f x
	Val i <*> _ = Val i
	Add e1 e2 <*> f = Add (e1 <*> f) (e2 <*> f)
  
instance Monad Expr where
	-- return :: a -> Expr a
	return = pure
	
	-- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
	Var v >>= f = f v
	Val v >>= _ = Val v
	Add e1 e2 >>= f = Add (e1 >>= f) (e2 >>= f)
