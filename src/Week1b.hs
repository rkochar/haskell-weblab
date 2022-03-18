module Week1b where

import Prelude hiding (replicate, length)

import GHC.List (length)

-- LIST COMPHREHENSION

{-
x : (xs ++ ys) == (x : xs) ++ ys

x : xs == [x] ++ xs

[] ++ xs == xs

x : [] == [x]
-}

evens :: [Int] -> [Int]
evens xs = [x | x <- xs, x `mod` 2 == 0]

countDownBy5 :: Int -> [Int]
countDownBy5 n = [x | x <- [n, n-5..0]]

sum_of_squares :: Int -> Int
sum_of_squares n = sum [x*x | x <- [1 .. n]]

replicate :: Int -> a -> [a]
replicate n e = [e | _ <- [1 .. n]]

remove :: Int -> [a] -> [a]
remove n xs = (take n xs) ++ (drop (n + 1) xs)

pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], x * x + y * y == z * z]

factors :: Int -> [Int]
factors n = [x | x <- [1 .. (n - 1)], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], sum (factors x) == x]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct ns ms = if length ms == length ns then sum [n * m | (n, m) <- zip ns ms] else 0 

riffle :: [a] -> [a] -> [a]
riffle ns ms = concat [[n, m] | (n, m) <- zip ns ms]

divisors :: Int -> [Int]
divisors n = [x | x <- [1 .. n], n `mod` x == 0]

grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x, y) | x <- [0 .. m], y<- [0 .. n]]

square :: Int -> [(Int,Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

-- Make histogram given a list of integers between 0 and 9
count :: Eq a => [a] -> a -> Int
count xs n = length [x | x <- xs, x == n]

maxTuple :: Ord t => [(a, t)] -> t -> t
maxTuple xs max = case xs of []            -> max
                             (x, y) : tail -> if y > max
                                              then maxTuple tail y
                                              else maxTuple tail max


histogramToString :: Ord a1 => [(a2, a1)] -> a1 -> [Char]
histogramToString xs max = case xs of []          -> ""
                                      (_, y) : tail -> if y >= max
                                      				   then "*" ++ histogramToString tail max
                                      				   else " " ++ histogramToString tail max

generateHistogram :: (Ord t, Num t) => [(a2, t)] -> t -> [Char]
generateHistogram xs max = if max > 0 
                           then histogramToString xs max ++ "\n" ++ generateHistogram xs (max - 1)
                           else "==========\n0123456789\n"

histogramFrequencies :: (Num a, Enum a, Eq a) => [a] -> [(a, Int)]
histogramFrequencies xs = [(x, count xs x) | x <- [0 .. 9]]

histogram :: [Int] -> String
histogram xs = generateHistogram frequencies  (maxTuple frequencies 0)
			   where frequencies = histogramFrequencies xs
