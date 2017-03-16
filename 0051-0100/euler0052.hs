
-- https://projecteuler.net/problem=52
--
-- It can be seen that the number, 125874, and its double, 251748, contain
-- exactly the same digits, but in a different order.
-- Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x,
-- contain the same digits.

import Data.List

solve :: Integer -> Bool
solve a = let
  t2 = sort $ show (2 * a)
  t3 = sort $ show (3 * a)
  t4 = sort $ show (4 * a)
  t5 = sort $ show (5 * a)
  t6 = sort $ show (6 * a)
  in t2 == t3 && t3 == t4 && t4 == t5 && t5 == t6

result :: [Integer] -> Integer
result numbers = head [i | i <- numbers, solve i]


main :: IO ()
main = print ("Result should be: " ++ show (142857 :: Integer) ++ ", is: " ++ show (result [1..]))
