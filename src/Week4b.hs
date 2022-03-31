module Week4b where

import Data.Monoid

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



data Op = Add Double | Subtract Double | Multiply Double | Divide Double | Sqrt
  deriving (Show)

opCost :: Op -> Sum Int
opCost (Add _) = Sum 1
opCost (Subtract _) = Sum 2
opCost (Multiply _) = Sum 5
opCost (Divide _) = Sum 10
opCost Sqrt = Sum 20

opLog :: Op -> String
opLog (Add x) = "Adding " ++ show x
opLog (Subtract x) = "Subtracting " ++ show x
opLog (Multiply x) = "Multiplying by " ++ show x
opLog (Divide x) = "Dividing by " ++ show x
opLog Sqrt = "Taking Square Root"

--applyOpLog :: Op -> Double -> Writer [String] Double
applyOpLog op d = opLog $ op d