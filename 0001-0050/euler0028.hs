-- https://projecteuler.net/problem=28
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


walk :: Integer -> Integer
walk 1 = 1
walk currentLevel
  | mod currentLevel 2 == 0 = error "invalid level number (must be odd)"
  | otherwise =
    let
      maxOnThisLevel = currentLevel ^ 2
      edgesValues = map (\n -> maxOnThisLevel - (n * (currentLevel - 1))) [0,1,2,3]
    in sum edgesValues + (walk (currentLevel - 2))

main :: IO ()
main = do
  --print ("Expected: " ++ show (101 :: Integer) ++ ", actual: " ++ show (walk 5))
  print ("Expected: " ++ show (669171001 :: Integer) ++ ", actual: " ++ show (walk 1001))
