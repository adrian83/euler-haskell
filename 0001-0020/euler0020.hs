-- https://projecteuler.net/problem=20
--
-- n! means n × (n − 1) × ... × 3 × 2 × 1
-- For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
-- and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
-- Find the sum of the digits in the number 100!

digits :: Integer -> [Integer]
digits 0 = []
digits i = mod i 10 : digits (quot i 10)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

digitsSum :: Integer -> Integer
digitsSum number = sum $ digits number

main :: IO ()
main = do
  --print ("Expected: " ++ show (27 :: Integer) ++ ", actual: " ++ show (digitsSum $ factorial 10))
  print ("Expected: " ++ show (648 :: Integer) ++ ", actual: " ++ show (digitsSum $ factorial 100))
