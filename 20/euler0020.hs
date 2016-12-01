-- https://projecteuler.net/problem=20
--
--n! means n × (n − 1) × ... × 3 × 2 × 1
--For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
--and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
--Find the sum of the digits in the number 100!



digits :: (Integral a) => a -> [a]
digits i = let
  r = mod i 10
  n = quot i 10
  in if n == 0 then [r] else r : digits n

silnia :: (Integral a) => a -> a
silnia 0 = 1
silnia n = n * silnia (n-1)





main = do
  print(sum $ digits $ silnia 100)
