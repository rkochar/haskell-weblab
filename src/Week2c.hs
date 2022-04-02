module Week2c where

import Test.QuickCheck
import Prelude
import Data.List (sort, delete)

{-
QUICKCHECK
-}

-- sum [] = 0
prop_sum_empty :: Bool
prop_sum_empty = 0 == sum []

-- sum [x] = x
prop_sum_singleton :: Int -> Bool
prop_sum_singleton x = sum [x] == x

-- sum (xs ++ ys) = sum xs + sum ys
prop_sum_concat :: (Eq a, Num a) => [a] -> [a] -> Bool
prop_sum_concat xs ys = sum (xs ++ ys) == sum xs + sum ys


sorted :: Ord a => [a] -> Bool
sorted (x:y:ys) = x <= y && sorted (y:ys)
sorted _        = True

prop_sort_sorted :: [Int] -> Bool
prop_sort_sorted xs = sorted (sort xs)

sameElements :: Eq a => [a] -> [a] -> Bool
sameElements [] [] = True
sameElements [] _  = False
sameElements (x : xs) ys = if elem x ys
						   then sameElements xs (delete x ys)
						   else False

prop_sort_sameElements :: Ord a => [a] -> Bool
prop_sort_sameElements xs = sameElements xs (sort xs)


prop_index :: [Int] -> Int -> Property
prop_index xs n = (n >= 0 && length xs > n) ==> xs !! n == head (drop n xs)

halve :: [Int] -> ([Int],[Int])
halve xs = splitAt l xs
				where
				  	l = length xs `div` 2

prop_halve_sameLength :: [Int] -> Property
prop_halve_sameLength xs = even (length xs) ==> sameLength $ halve xs
						       where
						 	       sameLength (a, b) = length a == length b


elemSorted :: Ord a => a -> [a] -> Bool
elemSorted x [] = False
elemSorted x (y:ys)
  | x <  y    = False
  | x == y    = True
  | otherwise = elemSorted x ys

-- broken: to test
insertSortedBroken :: Ord a => a -> [a] -> [a]
insertSortedBroken x ys = [x]

genSortedList :: Gen [Int]
genSortedList = foldr insertSortedBroken [] <$> (arbitrary :: Gen [Int])

prop_insertSorted :: Int -> Int -> Bool
prop_insertSorted x y = elemSorted y (insertSortedBroken x [y])




deleteSortedBroken :: Ord a => a -> [a] -> [a]
deleteSortedBroken x []     = []
deleteSortedBroken x (y:ys)
  | x <  y    = y:ys
  | x == y    = []
  | otherwise = y : deleteSortedBroken x ys

prop_insertSorted0 :: Int -> Property
prop_insertSorted0 x = forAll genSortedList (\xs -> elemSorted x (insertSorted x xs))

--prop_deleteSorted :: Int -> Property
--prop_deleteSorted x = forAll genSortedList (\xs -> deleteSortedBroken x xs == delete x (sorted xs))

prop_deleteSorted :: Int -> Int -> Property
prop_deleteSorted x y = x /= y ==> forAll genSortedList (\xs -> elemSorted x (deleteSortedBroken y (insertSorted x xs)))

insertSorted :: Ord a => a -> [a] -> [a]
insertSorted x [] = [x]
insertSorted x (y:ys)
  | x <  y    = x:y:ys
  | x == y    = y:ys
  | otherwise = y:(insertSorted x ys)

deleteSorted :: Ord a => a -> [a] -> [a]
deleteSorted x []     = []
deleteSorted x (y:ys)
  | x <  y    = y:ys
  | x == y    = ys
  | otherwise = y:(deleteSorted x ys)

mergeSorted :: Ord a => [a] -> [a] -> [a]
mergeSorted []     ys = ys
mergeSorted (x:xs) ys = insertSorted x (mergeSorted xs ys)

prop_insertDeleteSorted :: Int -> Property
prop_insertDeleteSorted x = forAll genSortedList (\xs -> not (elemSorted x (deleteSorted x (insertSorted x xs))))

prop_mergeSorted :: Property
prop_mergeSorted = forAll genSortedList (\xs -> forAll genSortedList (\ys -> isSorted (mergeSorted xs ys)))
  where
    isSorted :: [Int] -> Bool
    isSorted xs = sort xs == xs


