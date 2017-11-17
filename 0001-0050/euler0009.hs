-- https://projecteuler.net/problem=9
--
-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
-- a^2 + b^2 = c^2
-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

triplets :: Integer -> [(Integer, Integer, Integer)]
triplets maxC = [(a,b,c) | c <- [3,4..maxC], b <- [2,3..(c-1)], a <- [1,2..(b-1)]]

pythagorean :: (Integer, Integer, Integer) -> Bool
pythagorean (a, b, c) = (a ^ 2) + (b ^ 2) == (c ^ 2)

hasCircuit :: Integer -> (Integer, Integer, Integer) -> Bool
hasCircuit val (a, b, c) = val == (a + b + c)

pythagoreanTripletWithCircuit :: Integer -> Integer
pythagoreanTripletWithCircuit circuit = head $ (map (\(a,b,c) -> a*b*c) (filter pythagorean (filter (hasCircuit circuit) (triplets circuit))))

main :: IO ()
main = do
  --print ("Expected: " ++ show (60 :: Integer) ++ ", actual: " ++ show (pythagoreanTripletWithCircuit 12))
  print ("Expected: " ++ show (31875000 :: Integer) ++ ", actual: " ++ show (pythagoreanTripletWithCircuit 1000))
