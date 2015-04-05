module Basics where

import Prelude hiding (even,odd,sum,product,map,foldr)

---------------------
-- Introduce Tools --
---------------------

-- * GHCi
--   * :help, :load, :quit, :type
-- * Hoogle
-- * doctest


---------------------
-- Getting Started --
---------------------

-- * basic data types (Bool, Int, Float)
--   * arithmetic and boolean expressions
--   * if-then-else
-- * applying functions
--   * with one argument
--   * with multiple arguments
--   * infix vs. prefix application: operators are just functions!
--     * (+) x y = x + y
--     * f x y = x `f` y
-- * defining basic functions
--   * pattern matching
-- * anonymous functions


-- | Is this integer even?
even :: Int -> Bool
even x = (x `mod` 2) == 0


-- | Is this integer odd?
odd :: Int -> Bool
odd = not . even


-- | Is this number zero?
isZero :: Int -> Bool
isZero 0 = True
isZero _ = False


-------------------------
-- Lists and Recursion --
-------------------------

-- * lists and strings
--   * cons, nil, and syntactic sugar
--   * recursive functions
--   * higher-order functions


-- | Double all of the elements in a list.
doubleAll :: [Int] -> [Int]
doubleAll [] = []
doubleAll (x:xs) = x * 2 : doubleAll xs


-- | Flip all of the boolean values in a list.
notAll :: [Bool] -> [Bool]
notAll [] = []
notAll (x:xs) = not x : notAll xs


-- | Compute the sum of the elements in a list.
sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs


-- | Compute the product of the elements in a list.
product :: [Int] -> Int
product [] = 1
product (x:xs) = x * product xs



----------------------------
-- Higher-Order Functions --
----------------------------

-- * map and fold


-- | Map a function over the elements in a list.
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

-- | Fold a function over the elements in a list.
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc [] = acc
foldr f acc (x:xs) = f x (foldr f acc xs)

-- Reimplement the recursive functions from above using map and fold:

doubleAll' :: [Int] -> [Int]
doubleAll' xs = case xs of
				[] -> []
				(x:xs) -> map (\a -> a * 2) xs

notAll' :: [Bool] -> [Bool]
notAll' xs   =  map (not) xs

sum' :: [Int] -> Int
sum' xs      = case xs of
			 [] -> 0
			 (x:xs) -> foldr (+) 0 xs

product' :: [Int] -> Int
product' xs  = case xs of
			 [] -> 1
			 (x:xs) -> foldr (*) 1 xs
