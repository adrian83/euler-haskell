-- https://projecteuler.net/problem=36
--
--
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)

--putStrLn $ showHex 12 "" -- prints "c"
--putStrLn $ showIntAtBase 2 intToDigit 12 ""

isPalindrom :: (Eq a) => [a] -> Bool
isPalindrom [] = True
isPalindrom [_] = True
isPalindrom (x:xs) = (x == last xs) && isPalindrom (init xs)

digits :: Integer -> [Integer]
digits 0 = []
digits n = (mod n 10) : digits (quot n 10)

isPalindromic :: Integer -> Bool
isPalindromic n = isPalindrom (showIntAtBase 2 intToDigit n "") && isPalindrom (digits n)

sumOfPalindromics :: Integer -> Integer
sumOfPalindromics maxNumber = sum [i | i <- [1,2..maxNumber], isPalindromic i]


main = do
  print(showIntAtBase 2 intToDigit 12 "")
  print(sumOfPalindromics 1000000)
