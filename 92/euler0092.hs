
-- https://projecteuler.net/problem=92
--
-- A number chain is created by continuously adding the square of the digits in a number to form a new number until it has been seen before.
-- For example,
-- 44 → 32 → 13 → 10 → 1 → 1
-- 85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89
-- Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop. What is most amazing is that EVERY starting number will eventually arrive at 1 or 89.
-- How many starting numbers below ten million will arrive at 89?

sumOfSqares :: (Integral a) => [a] -> a
sumOfSqares [] = 0
sumOfSqares n = sum [i*i | i <- n]

digits :: (Integral a) => a -> [a]
digits i = let
  r = mod i 10
  n = quot i 10
  in if n == 0 then [r] else r : digits n

chainTo89 :: (Integral a) => a -> a
chainTo89 a = if a == 1 then 0 else (if a == 89 then 1 else chainTo89 $ sumOfSqares $ digits a)

calculate89 :: (Integral a) => [a] -> a
calculate89 a = sum [ chainTo89 i | i <- a ]

main = do
    print (calculate89 [1..100000])
