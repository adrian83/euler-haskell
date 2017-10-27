-- https://projecteuler.net/problem=39
--
-- If p is the perimeter of a right angle triangle with integral
-- length sides, {a,b,c}, there are exactly three solutions for p = 120.
-- {20,48,52}, {24,45,51}, {30,40,50}
-- For which value of p ≤ 1000, is the number of solutions maximised?

import Data.List
import Data.Ord

data Solution = Solution {
    perimiter :: Integer,
    count :: Int
  } deriving (Show, Eq)

instance Ord Solution where
  compare s1 s2 
    | count s1 > count s2 = LT
    | count s1 < count s2 = GT
    | otherwise = EQ



rightAngle :: (Integer, Integer, Integer) -> Bool
rightAngle (a, b, c) = (a * a) + (b * b) == (c * c)

triangles :: Integer -> [(Integer, Integer, Integer)]
triangles o = [ (a, b, c) | a <- [1..o], b <- [(a+1)..o], c <- [o - (a + b)], a + b > (quot c 2)]

rightAngleTriangles :: Integer -> [(Integer, Integer, Integer)]
rightAngleTriangles perimiter = filter rightAngle $ triangles perimiter

solutions :: Integer -> [Solution]
solutions maxPerimeter = [Solution {perimiter = p, count = length $ rightAngleTriangles p} | p <- [3..maxPerimeter]]

result ::Integer -> Solution
result maxPerimeter = head $ sort $ solutions maxPerimeter

main :: IO ()
main = do
  print ( result 1000 )
