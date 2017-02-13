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
result maxLevel level step curStep acc =
    if level > maxLevel then acc else (
      if curStep == 0 then result maxLevel (level+2) (step+2) 4 acc else result maxLevel level step (curStep-1) ((head acc + step) : acc)
    )

main :: IO ()
main = do
  print(sum $ result 5 3 2 4 [1])
  print(sum $ result 1001 3 2 4 [1])
