module Week2a where

import Prelude hiding (product, reverse, (^), and, concat, replicate, (!!), elem, sum, take, last)
import Data.List (delete)

{-
PATTERN MATCHING AND RECURSION
-}

third1 :: [a] -> a
third1 xs = head $ tail $ tail xs
third2 :: [a] -> a
third2 xs = xs !! 2
third3 :: [a] -> a
third3 xs = third3helper xs 3

third3helper :: [a] -> Int -> a
third3helper xs t = case xs of
                               x : tail -> if t == 1
                                		   then x	
                                		   else third3helper tail (t - 1)
                                		 
product :: Num p => [p] -> p
product xs = case xs of []       -> 1
                        x : tail -> x * product tail
                        
reverse :: [a] -> [a]
reverse xs = reverseHelper xs []

reverseHelper :: [a1] -> [a1] -> [a1]
reverseHelper [] x        = x
reverseHelper (x : xs) ys = reverseHelper xs (x : ys)

sumdown :: Int -> Int
sumdown n = sumdownHelper n 0

sumdownHelper :: (Eq t, Num t) => t -> t -> t
sumdownHelper 0 s = s
sumdownHelper n s = sumdownHelper (n - 1) (s + n)

(^) :: Int -> Int -> Int
_ ^ 0 = 1
0 ^ _ = 0
x ^ y = x * (x ^ (y - 1))

and :: [Bool] -> Bool
and bs = case bs of [] -> True
                    b : tail -> b && and tail 

concat :: [[a]] -> [a]
concat []           = []
concat (xs : xss)  = xs ++ concat xss

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate 1 k = [k]
replicate x k 
	| x > 0     = [k] ++ replicate (x - 1) k
    | otherwise = undefined
	
(!!) :: [a] -> Int -> a
(x : xs) !! 0 = x
(x : xs) !! k 
	| k > 0     = xs !! (k - 1)
	| otherwise = undefined
	
elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y : ys) = if x == y
                  then True
                  else elem x ys

sum :: [Int] -> Int
sum [] = 0
sum (x : xs) = x + sum xs

take :: Int -> [a] -> [a]
take 0 xs       = xs
take _ []       = []
take k (x : xs) = x : take (k - 1) xs

last :: [a] -> a
last [x]      = x
last (_ : xs) = last xs

power :: Integer -> Integer -> Integer
power n 0  = 1
power n k  = if even k 
             then power (n * n) (k `div` 2)
             else n * power n (k-1)
             
euclid :: (Ord t, Num t) => t -> t -> t
euclid a b = if a == b 
             then a
             else if a < b
             	  then euclid a (b - a)
             	  else euclid b a
             	  
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys) = if x < y
						  then x : merge xs (y : ys)
						  else y : merge (x : xs) ys

split :: [a] -> ([a], [a])
split xs = (take k xs, drop k xs)
           where k = length xs `div` 2


splitAlternate :: [a] -> ([a],[a])
splitAlternate xs = splitAlternateHelper xs [] []

splitAlternateHelper :: [a] -> [a] -> [a] -> ([a], [a])
splitAlternateHelper [] [] [] = ([], [])
splitAlternateHelper [x] ys zs = (x : ys, zs)
splitAlternateHelper [x1, x2] ys zs = (x1 : ys, x2 : zs)
splitAlternateHelper (x1 : x2 : xs) ys zs = splitAlternateHelper xs (x1 : ys) (x2: zs)

msort :: Ord a => [a] -> [a]
msort xs
  | length xs > 1 = let (x, y) = split xs
                    in merge (msort x) (msort y)
  | length xs == 1 = xs
  | otherwise = []
  
bagEqual :: (Eq a) => [a] -> [a] -> Bool
bagEqual [] [] = True
bagEqual [] _ = False
bagEqual _ [] = False
bagEqual (x : xs) ys = if elem x ys
                       then bagEqual xs (delete x ys)
                       else False
                       