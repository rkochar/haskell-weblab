module Week2b where

import Data.List hiding (dropWhile, takeWhile, filter, map)
import Prelude hiding (map, filter, all, any, takeWhile, dropWhile, curry, uncurry, iterate)
  
{-
HIGHER ORDER FUNCTIONS
-}

evens :: [Int] -> [Int]
evens = filter even
--evens = filter (\x -> x `mod` 2 == 0)

addMod3Is2 :: [Int] -> [Int]
addMod3Is2 = map (+3) . filter (\x -> x `mod` 3 == 2) 
--addMod3Is2 xs = [ x + 3 | x <- xs, x `mod` 3 == 2 ]

sumList :: [Int] -> Int
sumList = foldl (+) 0 -- or just sum
--sumList [] = 0
--sumList (x:xs) = x + sumList xs

allTrue :: [Bool] -> Bool
allTrue = foldl (&&) True -- or just and
--allTrue []     = True
--allTrue (b:bs) = b && allTrue bs

thrice :: (a -> a) -> a -> a
thrice f x = f . f $ f x

applyN :: Int -> (a -> a) -> a -> a
applyN n f x = foldr (\_ y -> f y) x [1 .. n] 
-- applyN n f x = iterate f x !! n

--map :: (a -> b) -> [a] -> [b]
--map f xs = [f x | x <- xs]

filter :: (a -> Bool) -> [a] -> [a]
filter p xs = [x | x <- xs, p x]

all :: (a -> Bool) -> [a] -> Bool
all p xs = foldl (&&) True $ map p xs

any :: (a -> Bool) -> [a] -> Bool
any p xs = or $ map p xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p (x : xs) = if p x
                       then x : takeWhile p xs
                       else [] 

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ []       = []
dropWhile p (x : xs) = if p x
                       then dropWhile p xs 
                       else x : xs

curry :: ((a, b) -> c) -> (a -> b -> c)
curry f x y = f (x, y) 

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f (x, y) = f x y

tryCurry :: Num a => a -> (a -> (a -> a))
tryCurry = \x -> (\y -> (\z -> x + y * z))

dec2int :: [Int] -> Int
dec2int = foldl (\x y -> x * 10 + y) 0

deduplicate :: (Eq a) => [a] -> [a]
deduplicate []     = []
deduplicate (x:xs) = x : deduplicate (filter (/= x) xs)

deduplicate_fast :: (Ord a) => [a] -> [a]
deduplicate_fast = map head . group . sort -- Bonus: replace this with a faster implementation

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)
               
int2bin :: Integral a => a -> [a]
int2bin xs = reverse (unfold (== 0) (`mod` 2) (`div` 2) xs)

map :: (a -> b) -> [a] -> [b]
map f = unfold null (f . head) tail

iterate :: (a -> a) -> a -> [a]
iterate = unfold (const False) id
-- iterate f = unfold (const False) id f

reindex :: (Int -> Int) -> [a] -> [a]
reindex f xs = map (\i -> findIndex i ixs) [0..(length xs)-1]
  where 
    -- The elements of xs paired with their new indices
    ixs = zip (map f [0..]) xs 
    
    findIndex i ((j,x):jxs)
      | i == j    = x
      | otherwise = findIndex i jxs