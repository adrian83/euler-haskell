-- https://projecteuler.net/problem=14
--
-- The following iterative sequence is defined for the set of positive integers:
-- n → n/2 (n is even)
-- n → 3n + 1 (n is odd)
-- Using the rule above and starting with 13, we generate the following sequence:
-- 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
-- It can be seen that this sequence (starting at 13 and finishing at 1)
-- contains 10 terms. Although it has not been proved yet (Collatz Problem), it
-- is thought that all starting numbers finish at 1.
-- Which starting number, under one million, produces the longest chain?
-- NOTE: Once the chain starts the terms are allowed to go above one million.

import Data.List
import Data.Ord

data NumberAndChainLen = NumberAndChainLen {
    number :: Integer,
    chainLen :: Integer
  } deriving (Show, Eq)

instance Ord NumberAndChainLen where
  compare s1 s2
    | chainLen s1 > chainLen s2 = LT
    | chainLen s1 < chainLen s2 = GT
    | otherwise = EQ

chain :: Integer -> Integer
chain 1 = 1
chain number = 1 + (if mod number 2 == 0 then chain (quot number 2) else chain ((3 * number) + 1))

candidates :: Integer -> [NumberAndChainLen]
candidates maxNumber = [NumberAndChainLen { number=n, chainLen=(chain n) } | n <- [1,2..maxNumber]]

longestChain :: Integer -> Integer
longestChain maxNumber = number (head (sort (candidates maxNumber)))

main :: IO ()
main = print ("Expected: " ++ show (837799 :: Integer) ++ ", actual: " ++ show (longestChain 1000000))
