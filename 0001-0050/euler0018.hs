-- https://projecteuler.net/problem=18
--
-- By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.
--    3
--   7 4     10 7
--  2 4 6   12 14 13
-- 8 5 9 3 8  5  9  3     20 19 23 16
-- That is, 3 + 7 + 4 + 9 = 23.
--
-- Find the maximum total from top to bottom of the triangle below:
--               75
--              95 64
--             17 47 82
--            18 35 87 10
--           20 04 82 47 65
--          19 01 23 75 03 34
--         88 02 77 73 07 63 67
--        99 65 04 28 06 16 70 92
--       41 41 26 56 83 40 80 70 33
--      41 48 72 33 47 32 37 16 94 29
--     53 71 44 65 25 43 91 52 97 51 14
--    70 11 33 28 77 73 17 78 39 68 17 57
--   91 71 52 38 17 14 91 43 58 50 27 29 48
--  63 66 04 68 89 53 67 30 73 16 69 87 40 31
-- 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
-- NOTE: As there are only 16384 routes, it is possible to solve this problem by trying every route. However, Problem 67, is the same
-- challenge with a triangle containing one-hundred rows; it cannot be solved by brute force, and requires a clever method! ;o)

merge :: [Integer] -> [Integer] -> [Integer]
merge [] [] = []
merge upper lower
  | length upper == 1 && length lower /= 1     = (head upper + head lower) : merge upper (tail lower)
  | length upper == 1 && length lower == 1     = [head upper + head lower]
  | length lower > length upper                = (head upper + head lower) : merge upper (tail lower)
  | otherwise                                  = ((max (upper !! 0) (upper !! 1)) + head lower) : merge (tail upper) (tail lower)

calculateLongestPaths :: [[Integer]] -> [Integer]
calculateLongestPaths [a] = a
calculateLongestPaths [a, b] = merge a b
calculateLongestPaths (x:y:xs) = calculateLongestPaths ((merge x y):xs)

longestPath :: [[Integer]] -> Integer
longestPath levels = maximum $ calculateLongestPaths levels

main :: IO ()
main = do

  --let triangle1 = [[3],[7,4],[2,4,6],[8,5,9,3]]
  --let res1 = longestPath triangle1
  --print ("Result should be: " ++ show (23 :: Integer) ++ ", is: " ++ show res1)

  let triangle2 = [[75],[95,64],[17,47,82],[18,35,87,10],[20,4,82,47,65],[19,01,23,75,03,34],[88,02,77,73,07,63,67],[99,65,04,28,06,16,70,92],[41,41,26,56,83,40,80,70,33],[41,48,72,33,47,32,37,16,94,29],[53,71,44,65,25,43,91,52,97,51,14],[70,11,33,28,77,73,17,78,39,68,17,57],[91,71,52,38,17,14,91,43,58,50,27,29,48],[63,66,04,68,89,53,67,30,73,16,69,87,40,31],[04,62,98,27,23,09,70,98,73,93,38,53,60,04,23]]
  let result2 = longestPath triangle2
  print ("Expected: " ++ show (1074 :: Integer) ++ ", actual: " ++ show result2)
