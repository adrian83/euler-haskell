-- https://projecteuler.net/problem=48

-- The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
-- Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

sumOfPowers :: Integer -> Integer
sumOfPowers 1 = 1
sumOfPowers number = number^number + sumOfPowers (number-1)

result :: Integer -> Integer -> Integer
result digits lenOfSum = mod (sumOfPowers lenOfSum) (10^digits)

main :: IO ()
main = print ("Result should be: " ++ show (9110846700 :: Integer) ++ ", is: " ++ show (result 10 1000))
