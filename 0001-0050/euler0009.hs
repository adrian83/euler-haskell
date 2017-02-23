-- https://projecteuler.net/problem=9
--
-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
-- a^2 + b^2 = c^2
-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

tProd :: (Integer, Integer, Integer) -> Integer
tProd (a,b,c) = a*b*c

result :: Integer -> Integer
result circuit = tProd $ head $ calculate circuit (quot circuit 2) 0 0 []

calculateA :: Integer -> Integer -> (Integer, Bool)
calculateA c b
  | a2 /= a^(2::Integer) || a > b = (a, False)
  | otherwise = (a, True)
  where
    a2 = c^(2::Integer) - b^(2::Integer)
    a = floor $ sqrt $ fromIntegral a2

calculate :: Integer -> Integer -> Integer -> Integer -> [(Integer, Integer, Integer)] -> [(Integer, Integer, Integer)]
calculate circuit maxC c b acc
  | maxC == c = acc
  | b <= halfC = calculate circuit maxC c (halfC+1) acc
  | b >= c = calculate circuit maxC (c+1) 0 acc
  | correctA && a < b && a + b + c == circuit = calculate circuit maxC (c+1) 0 ((c,b,a):acc)
  | otherwise = calculate circuit maxC c (b+1) acc
  where
    halfC = quot c 2
    aTuple = calculateA c b
    a = fst aTuple
    correctA = snd aTuple


main :: IO ()
main = do
  print ("Result should be: " ++ show (60 :: Integer) ++ ", is: " ++ show (result 12))
  print ("Result should be: " ++ show (31875000 :: Integer) ++ ", is: " ++ show (result 1000))
