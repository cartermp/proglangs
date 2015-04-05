-- | Some infinite lists.
module InfList where

import Prelude hiding (map,succ,zipWith)



-- | An infinite list of ones.
--   >>> take 5 ones
--   [1,1,1,1,1]
ones :: [Int]
ones = 1 : ones



-- | The natural numbers.
--   >>> take 5 nats
--   [0,1,2,3,4]
nats :: [Int]
nats = 0 : map succ nats

-- | Map a function over all of the elements of a list.
--   (In the Prelude)
map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (a:as) = f a : map f as

-- | Increment an integer.
--   (In the Prelude)
succ :: Int -> Int
succ n = n + 1



-- | The even numbers.
--   >>> take 5 evens
--   [0,2,4,6,8]
evens :: [Int]
evens = filter (even) nats



-- | The Fibonacci sequence.
--   >>> take 7 fibs
--   [1,1,2,3,5,8,13]
fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- head (a:_)  = a
-- tail (_:as) = as

-- | Merge two lists element-wise. The result list is produced
--   by applying the given merge function to corresponding
--   elements of the argument lists.
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ []     []     = []
zipWith f (a:as) (b:bs) = f a b : zipWith f as bs