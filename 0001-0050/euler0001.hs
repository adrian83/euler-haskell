-- https://projecteuler.net/problem=1
--
-- If we list all the natural numbers below 10 that are multiples of 3 or 5,
-- we get 3, 5, 6 and 9. The sum of these multiples is 23.
-- Find the sum of all the multiples of 3 or 5 below 1000.


result :: Integer -> Integer
result maxNumber = sum [x | x <- [1..(maxNumber-1)], mod x 3 == 0 || mod x 5 ==0]

main :: IO ()
main = do
  print ("Result should be: " ++ show (23 :: Integer) ++ ", is: " ++ show (result 10))
  print ("Result should be: " ++ show (233168 :: Integer) ++ ", is: " ++ show (result 1000))
