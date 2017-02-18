-- https://projecteuler.net/problem=9
--
-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
-- a^2 + b^2 = c^2
-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

thd :: (Integer, Integer, Integer) -> Integer
thd (_,_,c) = c

generate :: Integer -> Integer -> (Integer, Integer, Integer)
generate circuit a = [[a,b,(circuit - (a + b))] | b <- [1,2..circuit], a+b+(circuit - (a + b)) == circuit]


-- pythagoreanTriplets returns list of pythagorean triplets for which sum of all edges is equal to 1st arg.
pythagoreanTriplets :: Integer -> [[Integer]]
pythagoreanTriplets s = [[a,b,c] | a <- [1,2..s], b <- [2,3..s], c <- [3,4..s], c > b, b > a, c^2 == a^2 + b^2, s == c + a + b]

pythagoreanTriplets2 :: Integer -> [[Integer]]
pythagoreanTriplets2 s =

  [  | a <- [1,2..s], b <- [2,3..s], c <- [3,4..s], c > b, b > a, c^2 == a^2 + b^2, s == c + a + b]

main :: IO ()
main = do
  print( product $ head $ pythagoreanTriplets 1000 )
