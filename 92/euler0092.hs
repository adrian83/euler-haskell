
-- https://projecteuler.net/problem=92
--
-- A number chain is created by continuously adding the square of the digits in a number to form a new number until it has been seen before.
-- For example,
-- 44 → 32 → 13 → 10 → 1 → 1
-- 85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89
-- Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop. What is most amazing is that EVERY starting number will eventually arrive at 1 or 89.
-- How many starting numbers below ten million will arrive at 89?

sumOfSqares :: [Integer] -> Integer
sumOfSqares [] = 0
sumOfSqares n = sum [i*i | i <- n]

digits :: Integer -> [Integer]
digits 0 = []
digits i = (mod i 10) : digits (quot i 10)

chainTo89 :: Integer -> Integer
chainTo89 0 = 0
chainTo89 89 = 1
chainTo89 a = chainTo89 $ sumOfSqares $ digits a

calculate89 :: [Integer] -> Integer
calculate89 a = sum [ chainTo89 i | i <- a ]

main = do
    print (calculate89 [1..100000])
