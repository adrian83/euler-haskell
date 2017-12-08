-- https://projecteuler.net/problem=52
--
-- It can be seen that the number, 125874, and its double, 251748, contain
-- exactly the same digits, but in a different order.
-- Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x,
-- contain the same digits.

import Data.List

sameDigits :: [Integer] -> Bool
sameDigits numbers =
  let
    sortedStrings = map (\numb -> sort $ show numb) numbers
    first = read (head sortedStrings) :: Integer
  in foldl (\a b -> a && first == b) True (map read (tail sortedStrings))

genMultiplications :: Integer -> [Integer] -> [Integer]
genMultiplications number multiplications = map (\n -> n * number) multiplications

smallestWithSameDigits :: [Integer] -> Integer
smallestWithSameDigits numbers = head $ filter (\n -> sameDigits (genMultiplications n [2,3..6])) numbers

main :: IO ()
main = print ("Expected: " ++ show (142857 :: Integer) ++ ", actual: " ++ show (smallestWithSameDigits [1..]))
