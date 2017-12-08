-- https://projecteuler.net/problem=48

-- The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
-- Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

sumOfPowers :: Integer -> Integer
sumOfPowers 1 = 1
sumOfPowers number = number ^ number + sumOfPowers (number - 1)

lastDigits :: Integer -> Integer -> Integer
lastDigits number numberOfDigits = mod number (10 ^ numberOfDigits)

main :: IO ()
main = print ("Expected: " ++ show (9110846700 :: Integer) ++ ", actual: " ++ show (lastDigits (sumOfPowers 1000) 10 ))
