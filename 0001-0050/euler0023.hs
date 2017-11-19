-- https://projecteuler.net/problem=23
--
-- A perfect number is a number for which the sum of its proper divisors is exactly
-- equal to the number. For example, the sum of the proper divisors of 28 would
-- be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
--
-- A number n is called deficient if the sum of its proper divisors is less
-- than n and it is called abundant if this sum exceeds n.
--
-- As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
-- number that can be written as the sum of two abundant numbers is 24. By
-- mathematical analysis, it can be shown that all integers greater than 28123
-- can be written as the sum of two abundant numbers. However, this upper limit
-- cannot be reduced any further by analysis even though it is known that the
-- greatest number that cannot be expressed as the sum of two abundant numbers
-- is less than this limit.
--
-- Find the sum of all the positive integers which cannot be written as the
-- sum of two abundant numbers.

import Data.List

removeDuplicates :: [Integer] -> [Integer]
removeDuplicates [a] = [a]
removeDuplicates [a,b] = if a == b then [a] else [a,b]
removeDuplicates (a:b:xs) = if a == b then (removeDuplicates (b:xs)) else a:(removeDuplicates (b:xs))

sumsOfTwoAbundants :: [Integer] -> Integer -> [Integer]
sumsOfTwoAbundants [] _ = []
sumsOfTwoAbundants abundants maxx = filter (<=maxx) (map (\n -> head abundants + n) abundants) ++ sumsOfTwoAbundants (tail abundants) maxx

sumOfAllWhichAreNotTwoAbundantsSum :: [Integer] -> Integer -> Integer
sumOfAllWhichAreNotTwoAbundantsSum abundants maxx = sum [1,2..maxx] - sum (removeDuplicates $ sort $ sumsOfTwoAbundants abundants (maxx))

main :: IO ()
main = do
  let maxx = 28123

  f <- readFile "../abundants/abundants"
  let abundants = filter (<=maxx) [read s :: Integer | s <- lines f]

  print ("Expected: " ++ show (4179871 :: Integer) ++ ", actual: " ++ show (sumOfAllWhichAreNotTwoAbundantsSum abundants maxx))
