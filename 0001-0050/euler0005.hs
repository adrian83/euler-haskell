-- https://projecteuler.net/problem=5
--
-- 2520 is the smallest number that can be divided by each of the numbers
-- from 1 to 10 without any remainder.
-- What is the smallest positive number that is evenly divisible by all of
-- the numbers from 1 to 20?


isDivisable :: Integer -> [Integer] -> Bool
isDivisable _ [] = True
isDivisable n (x:xs) =  mod n x == 0 && isDivisable n xs

smallestDivisableBy :: [Integer] -> Integer
smallestDivisableBy x = head [i | i <- [step, 2*step..], isDivisable i x]
  where step = head x


main :: IO ()
main = do
  print ("Result should be: " ++ show (2520 :: Integer) ++ ", is: " ++ show (smallestDivisableBy [10,9..1]))
  print ("Result should be: " ++ show (232792560 :: Integer) ++ ", is: " ++ show (smallestDivisableBy [20,19..1]))
