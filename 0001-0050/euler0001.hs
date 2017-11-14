-- https://projecteuler.net/problem=1
--
-- If we list all the natural numbers below 10 that are multiples of 3 or 5,
-- we get 3, 5, 6 and 9. The sum of these multiples is 23.
-- Find the sum of all the multiples of 3 or 5 below 1000.

divisableByAny :: Integer -> [Integer] -> Bool
divisableByAny number divisors = any (\b -> b) (map (\d -> mod number d == 0) divisors)

sumDivisables :: Integer -> [Integer] -> Integer
sumDivisables maxNumber divisors = sum (filter (\x -> divisableByAny x divisors) [1,2..maxNumber-1])

main :: IO ()
main = do
  --print ("Expected: " ++ show (23 :: Integer) ++ ", actual: " ++ show (sumDivisables 10 [3,5]))
  print ("Expected: " ++ show (233168 :: Integer) ++ ", actual: " ++ show (sumDivisables 1000 [3,5]))
