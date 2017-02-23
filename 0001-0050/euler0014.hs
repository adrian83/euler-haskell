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

-- chain returns length of the chain for given number (1st arg). Second arg contains current length.
chain :: Integer -> Integer -> Integer
chain n r
  | n == 1       = r
  | mod n 2 == 0 = chain (quot n 2) (r+1)
  | otherwise    = chain ((3*n)+1) (r+1)


longest :: Integer -> Integer -> (Integer, Integer) -> Integer
longest maks current best
  | current == maks  = fst best
  | collz > snd best = longest maks (current+1) (current,collz)
  | otherwise        = longest maks (current+1) best
  where collz = chain current 1

result :: Integer -> Integer
result maxNumber = longest maxNumber 1 (0,0)


main :: IO ()
main = print ("Result should be: " ++ show (837799 :: Integer) ++ ", is: " ++ show (result 1000000))
