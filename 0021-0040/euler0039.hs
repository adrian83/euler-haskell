-- https://projecteuler.net/problem=39
--
-- If p is the perimeter of a right angle triangle with integral length
-- sides, {a,b,c}, there are exactly three solutions for p = 120.
--     {20,48,52}, {24,45,51}, {30,40,50}
-- For which value of p ≤ 1000, is the number of solutions maximised?


import qualified Data.Map.Lazy as Map

isRightAngleTriangle :: Integer -> Integer -> Integer -> Bool
isRightAngleTriangle a b c = a^2 + b^2 == c^2


allTriangles :: Integer -> Integer -> Integer -> [(Integer, Integer, Integer)]
allTriangles maxPerimeter a b
    | perimiter > maxPerimeter && a == (b-1)   = []
    | perimiter > maxPerimeter && a /= (b-1)   = allTriangles maxPerimeter (a+1) (a+2)
    | isRightAngle                             = (a, b, c) : allTriangles maxPerimeter (a) (b+1)
    | otherwise                                = allTriangles maxPerimeter (a) (b+1)
    where
        c = floor (sqrt $ fromIntegral ((a^2) + (b^2)))
        perimiter = a + b + c
        isRightAngle = isRightAngleTriangle a b c


calcPerimiter :: (Integer, Integer, Integer) -> Integer
calcPerimiter (a, b, c) = a + b + c


groupByPerimiter :: [(Integer, Integer, Integer)] -> Map.Map Integer Integer -> Map.Map Integer Integer
groupByPerimiter [] m = m
groupByPerimiter l m = case prevResult of
    Just i -> groupByPerimiter (tail l) (Map.insert r (i+1) m)
    Nothing -> groupByPerimiter (tail l) (Map.insert r 1 m)
    where
        r = calcPerimiter (head l)
        prevResult = Map.lookup r m


longest :: [(Integer, Integer)] -> (Integer, Integer)
longest [] = (0, 0)
longest l = if snd h > snd best then h else best
    where
        h = head l
        best = longest (tail l)


result :: Integer -> Integer
result maxPerimiter = fst $ longest $ Map.toList (groupByPerimiter (allTriangles maxPerimiter 1 2) Map.empty)


main :: IO ()
main = print ("Expected: " ++ show (840 :: Integer) ++ ", actual: " ++ show (result 1000))
