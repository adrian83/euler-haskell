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

import qualified Data.Map.Lazy as Map

data NumberAndChainLen = NumberAndChainLen {
    number :: Integer,
    chainLen :: Integer
  } deriving (Show, Eq)


chain :: Integer -> Map.Map Integer Integer -> Integer
chain 1 _ = 1
chain number dict =
  case Map.lookup number dict of
    Just n -> n
    Nothing -> 1 + (if mod number 2 == 0 then chain (quot number 2) dict else chain ((3 * number) + 1) dict)


longestChain :: Integer -> Integer -> Map.Map Integer Integer -> NumberAndChainLen -> Integer
longestChain current maxNumber cache best
  | current >= maxNumber = number best
  | otherwise = longestChain (current+1) maxNumber newCache newBest
  where
    chainL = chain current cache
    newCache = Map.insert current chainL cache
    cand = NumberAndChainLen { number=current, chainLen=chainL }
    newBest = if chainLen cand > chainLen best then cand else best


result :: Integer -> Integer
result maxx = longestChain 1 maxx Map.empty (NumberAndChainLen { number=1, chainLen=1 })


main :: IO ()
main = print ("Expected: " ++ show (837799 :: Integer) ++ ", actual: " ++ show (result 1000000))
