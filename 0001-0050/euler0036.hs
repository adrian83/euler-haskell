-- https://projecteuler.net/problem=36
--
-- The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.
-- Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
-- (Please note that the palindromic number, in either base, may not include leading zeros.)


import Numeric (showIntAtBase)
import Data.Char (intToDigit)

isPalindrom :: (Eq a) => [a] -> Bool
isPalindrom [] = True
isPalindrom [_] = True
isPalindrom (x:xs) = (x == last xs) && isPalindrom (init xs)

digits :: Integer -> [Integer]
digits 0 = []
digits n = mod n 10 : digits (quot n 10)

isPalindromic :: Integer -> Bool
isPalindromic n = isPalindrom (showIntAtBase 2 intToDigit n "") && isPalindrom (digits n)

sumOfPalindromics :: Integer -> Integer
sumOfPalindromics maxNumber = sum [i | i <- [1,2..maxNumber+1], isPalindromic i]

main :: IO ()
main = print ("Result should be: " ++ show (872187 :: Integer) ++ ", is: " ++ show (sumOfPalindromics 100000))
