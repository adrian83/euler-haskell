-- https://projecteuler.net/problem=6
--
-- The sum of the squares of the first ten natural numbers is,
-- 1^2 + 2^2 + ... + 10^2 = 385
-- The square of the sum of the first ten natural numbers is,
-- (1 + 2 + ... + 10)^2 = 552 = 3025
-- Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.
-- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

-- sumOfSquares returns sum of squares of numbers from 1 to given max (1st arg).
sumOfSquares :: Integer -> Integer
sumOfSquares maxx = sum [ a ^ 2 | a <- [1..maxx] ]

-- squareOfSums returns square of sums of numbers from 1 to given max (1st arg).
squareOfSums :: Integer -> Integer
squareOfSums maxx = sum [1,2..maxx] ^ (2 :: Integer)

difference :: Integer -> Integer
difference maxx = squareOfSums maxx - sumOfSquares maxx

main :: IO ()
main = do
  --print ("Expected: " ++ show (2640 :: Integer) ++ ", actual: " ++ show (difference 10))
  print ("Expected: " ++ show (25164150 :: Integer) ++ ", actual: " ++ show (difference 100))
