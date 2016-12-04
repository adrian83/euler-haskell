
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
sumOfSquares maks = sum [ a*a | a <- [1..maks] ]

-- squareOfSums returns square of sums of numbers from 1 to given max (1st arg).
squareOfSums :: Integer -> Integer
squareOfSums maks = let s = sum [ a | a <- [1..maks] ] in s*s

main :: IO ()
main = do
  print ( squareOfSums 10 - sumOfSquares 10 )
  print ( squareOfSums 100 - sumOfSquares 100 )
