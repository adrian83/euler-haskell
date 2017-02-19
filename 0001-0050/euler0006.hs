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
squareOfSums maks = sum [1,2..maks] ^ (2 :: Integer)

main :: IO ()
main = do
  print ("Result should be: " ++ show (2640 :: Integer) ++ ", is: " ++ show (squareOfSums 10 - sumOfSquares 10))
  print ("Result should be: " ++ show (25164150 :: Integer) ++ ", is: " ++ show (squareOfSums 100 - sumOfSquares 100))
