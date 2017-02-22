-- https://projecteuler.net/problem=25
--
-- Starting with the number 1 and moving to the right in a clockwise
-- direction a 5 by 5 spiral is formed as follows:
-- 21 22 23 24 25
-- 20  7  8  9 10
-- 19  6  1  2 11
-- 18  5  4  3 12
-- 17 16 15 14 13
-- It can be verified that the sum of the numbers on the diagonals is 101.
-- What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral
-- formed in the same way?

result :: Integer -> Integer -> Integer -> Integer -> [Integer] -> [Integer]
result maxLevel level step curStep acc
  | level > maxLevel = acc
  | curStep == 0     = result maxLevel (level+2) (step+2) 4 acc
  | otherwise        = result maxLevel level step (curStep-1) ((head acc + step) : acc)


main :: IO ()
main = do
  print ("Result should be: " ++ show (101 :: Integer) ++ ", is: " ++ show (sum $ result 5 3 2 4 [1]))
  print ("Result should be: " ++ show (669171001 :: Integer) ++ ", is: " ++ show (sum $ result 1001 3 2 4 [1]))
