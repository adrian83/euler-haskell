-- https://projecteuler.net/problem=1
--
-- If we list all the natural numbers below 10 that are multiples of 3 or 5,
-- we get 3, 5, 6 and 9. The sum of these multiples is 23.
-- Find the sum of all the multiples of 3 or 5 below 1000.

result :: Integer -> Integer
result maxNumber = sum $ filter (\x -> mod x 3 == 0 || mod x 5 ==0) [1..(maxNumber-1)]

main :: IO ()
main = do
  --print ("Expected: " ++ show (23 :: Integer) ++ ", actual: " ++ show (result 10))
  print ("Expected: " ++ show (233168 :: Integer) ++ ", actual: " ++ show (result 1000))
