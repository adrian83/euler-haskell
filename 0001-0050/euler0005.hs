-- https://projecteuler.net/problem=5
--
-- 2520 is the smallest number that can be divided by each of the numbers
-- from 1 to 10 without any remainder.
-- What is the smallest positive number that is evenly divisible by all of
-- the numbers from 1 to 20?

divisableByAll :: Integer -> [Integer] -> Bool
divisableByAll number divisors = all (\b -> b) (map (\d -> mod number d == 0) divisors)

smallestDivisableBy :: [Integer] -> Integer
smallestDivisableBy x = head [i | i <- [step, 2*step..], divisableByAll i x]
  where step = maximum x

main :: IO ()
main = do
  --print ("Expected: " ++ show (2520 :: Integer) ++ ", actual: " ++ show (smallestDivisableBy [1,2..10]))
  print ("Expected: " ++ show (232792560 :: Integer) ++ ", actual: " ++ show (smallestDivisableBy [1,2..20]))
