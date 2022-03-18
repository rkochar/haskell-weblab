module Week1a where

import Prelude hiding (init)

import Data.List

add_and_double x y = 2 * (x + y)

n =  a `div` length xs
     where
          a = 10
          xs = [1,2,3,4,5]

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) =
  qsort smaller ++ [x] ++ qsort larger
    where
      smaller = [ y | y <- xs , y <= x ]
      larger  = [ y | y <- xs , y > x  ]

bools :: [Bool]
bools = [True]

nums :: [[Int]]
nums = [[42, 4, 2], [3]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a,a)
copy x = (x, x)

halve :: [Int] -> ([Int],[Int])
halve xs = (take l xs, drop l xs)
				where
				  	l = length xs `div` 2

init xs = reverse $ tail $ reverse xs

safetail xs
	| xs == [] = []
	| otherwise = tail xs


quadratic_solutions :: Double -> Double -> Double -> [Double]
quadratic_solutions a b c = if d > 0
							then [(-b + d) / (2 * a), (-b - d) / (2 * a)]
							else if d == 0
							then [-b / (2 * a), -b / (2 * a)]
							else []
							where
							  	d = sqrt (b * b - 4 * a * c)

luhnDouble :: Int -> Int
luhnDouble x = if x * 2 > 9
			   then x * 2 - 9
			   else x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (d + (luhnDouble c) + b + (luhnDouble a)) `mod` 10 == 0

luhnFinal :: Int -> Int -> Int -> Int
luhnFinal a b c = 10 - ((luhnDouble c + luhnDouble a + b) `mod` 10)
