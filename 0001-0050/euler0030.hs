-- https://projecteuler.net/problem=30
--
-- Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:
--      1634 = 14 + 64 + 34 + 44
--      8208 = 84 + 24 + 04 + 84
--      9474 = 94 + 44 + 74 + 44
-- As 1 = 14 is not a sum it is not included.
-- The sum of these numbers is 1634 + 8208 + 9474 = 19316.
-- Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.


digits :: Integer -> [Integer]
digits 0 = []
digits i = mod i 10 : digits (quot i 10)

isSumOfItsDigits :: Integer -> Integer -> Bool
isSumOfItsDigits number power = number == sum [i^(power::Integer) | i <- digits number]

result :: Integer -> Integer -> [Integer]
result maxNumber power = [i | i <- [2,3..maxNumber], isSumOfItsDigits i power]

main :: IO ()
main = print ("Expected: " ++ show (443839 :: Integer) ++ ", actual: " ++ show (sum $ result 1000000 5))
