-- https://projecteuler.net/problem=34
--
-- 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
-- Find the sum of all numbers which are equal to the sum of the factorial of their digits.
-- Note: as 1! = 1 and 2! = 2 are not sums they are not included.


digits :: Integer -> [Integer]
digits 0 = []
digits i = mod i 10 : digits (quot i 10)

factorial :: Integer -> Integer
factorial 0 = 1
factorial 1 = 1
factorial a = a * factorial (a-1)

factorialsSum :: [Integer] -> Integer
factorialsSum numbers = sum $ map factorial numbers

result :: Integer -> Integer
result maxNumber = sum [i | i <- [3,4..maxNumber], i == factorialsSum (digits i)]

main :: IO ()
main = print ("Result should be: " ++ show (40730 :: Integer) ++ ", is: " ++ show (result 10000000))
