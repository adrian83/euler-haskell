-- https://projecteuler.net/problem=20
--
-- n! means n × (n − 1) × ... × 3 × 2 × 1
-- For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
-- and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
-- Find the sum of the digits in the number 100!



digits :: Integer -> [Integer]
digits 0 = []
digits i = (mod i 10) : digits (quot i 10)


silnia :: Integer -> Integer
silnia 0 = 1
silnia n = n * silnia (n-1)


main = do
  print(sum $ digits $ silnia 10)
  print(sum $ digits $ silnia 100)
