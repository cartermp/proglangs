module HW1 where

import Prelude hiding (Enum(..), sum)
import Data.List hiding (sum)


--
-- * Part 1: Natural numbers
--

-- | The natural numbers.
data Nat = Zero
         | Succ Nat
         deriving (Eq,Show)

-- | The number 0.
zero :: Nat
zero = Zero

-- | The number 1.
one :: Nat
one = Succ zero

-- | The number 2.
two :: Nat
two = Succ one

-- | The number 3.
three :: Nat
three = Succ two

-- | The number 4.
four :: Nat
four = Succ three


-- | The predecessor of a natural number.
--   
--   >>> pred zero
--   Zero
--   
--   >>> pred three
--   Succ (Succ Zero)
pred :: Nat -> Nat
pred nat = case nat of
           Zero -> Zero
           (Succ innerNat) -> innerNat


-- | True if the given value is zero.
--
--   >>> isZero zero
--   True
--
--   >>> isZero two
--   False
--
isZero :: Nat -> Bool
isZero nat = case nat of
             Zero -> True
             _ -> False


-- | Convert a natural number to an integer.
--
--   >>> toInt zero
--   0
--
--   >>> toInt three
--   3
toInt :: Nat -> Int
toInt nat = case nat of
            Zero -> 0
            _ -> 1 + toInt (pred nat)


-- | Add two natural numbers.
--
--   >>> add one two
--   Succ (Succ (Succ Zero))
--
--   >>> add zero one == one
--   True
--
--   >>> add two two == four
--   True
--
--   >>> add two three == add three two
--   True
add :: Nat -> Nat -> Nat
add lhs rhs = case lhs of
              Zero -> rhs
              _ -> Succ (add (pred lhs) rhs)


-- | Subtract the second natural number from the first. Return zero
--   if the second number is bigger.
--
--   >>> sub two one
--   Succ Zero
--   
--   >>> sub three one
--   Succ (Succ Zero)
--
--   >>> sub one one
--   Zero
--
--   >>> sub one three
--   Zero
sub :: Nat -> Nat -> Nat
sub lhs rhs = if (gt rhs lhs) then Zero
              else case rhs of
                   Zero -> lhs
                   _ -> sub (pred lhs) (pred rhs)


-- | Is the left value greater than the right?
--
--   >>> gt one two
--   False
--
--   >>> gt two one
--   True
--
--   >>> gt two two
--   False
gt :: Nat -> Nat -> Bool
gt lhs rhs = (toInt lhs) > (toInt rhs)


-- | Multiply two natural numbers.
--
--   >>> mult two zero
--   Zero
--
--   >>> mult zero three
--   Zero
--
--   >>> toInt (mult two three)
--   6
--
--   >>> toInt (mult three three)
--   9
--   >>> mult two two == add four four
--   True
multInner :: Nat -> Nat -> Nat
multInner lhs rhs = case lhs of
                    Zero -> Zero
                    _ -> add (multInner (pred lhs) rhs) rhs

mult :: Nat -> Nat -> Nat
mult lhs rhs = if (lhs == Zero || rhs == Zero) then Zero
               else multInner lhs rhs


-- | Compute the sum of a list of natural numbers.
--
--   >>> sum []
--   Zero
--   
--   >>> sum [one,zero,two]
--   Succ (Succ (Succ Zero))
--
--   >>> toInt (sum [one,two,three])
--   6
sum :: [Nat] -> Nat
sum [] = Zero
sum items = foldr (add) zero items


-- | An infinite list of all of the *odd* natural numbers, in order.
--
--   >>> map toInt (take 5 odds)
--   [1,3,5,7,9]
--
--   >>> toInt (sum (take 100 odds))
--   10000
nats :: [Nat]
nats = Zero : map (\x -> add x one) nats

odds :: [Nat]
odds = filter (\x -> odd (toInt x)) nats



--
-- * Part 2: Run-length lists
--


-- | Convert a regular list into a run-length list.
--
--   >>> compress [1,1,1,2,3,3,3,1,2,2,2,2]
--   [(3,1),(1,2),(3,3),(1,1),(4,2)]
-- 
--   >>> compress "Mississippi"
--   [(1,'M'),(1,'i'),(2,'s'),(1,'i'),(2,'s'),(1,'i'),(2,'p'),(1,'i')]
compress :: Eq a => [a] -> [(Int,a)]
compress = map (\lst -> (length lst, head lst)) . group


-- | Convert a run-length list back into a regular list.
--
--   >>> decompress [(5,'a'),(3,'b'),(4,'c'),(1,'a'),(2,'b')]
--   "aaaaabbbccccabb"
generate :: a -> [a]
generate a = a : generate a

expand :: (Int, a) -> [a]
expand item = take (fst item) (generate (snd item))

decompress :: [(Int,a)] -> [a]
decompress = concat . map (\x -> expand x)