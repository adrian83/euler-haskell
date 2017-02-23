-- https://projecteuler.net/problem=41

-- We shall say that an n-digit number is pandigital if it makes use of all
-- the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital
-- and is also prime.
-- What is the largest n-digit pandigital prime that exists?

import Data.List

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [a] = [a]
removeDuplicates elements = if f == head t then removeDuplicates (f : tail t) else f : removeDuplicates t
  where
    f = head elements
    t = tail elements


isPandigital :: Integer -> Bool
isPandigital number
  | '0' `elem` numberAsStr = False
  | (orgLen == length uniqueDigits) && (last uniqueDigits == head (show orgLen)) = True
  | otherwise = False
  where
    numberAsStr = show number
    orgLen = length numberAsStr
    uniqueDigits = removeDuplicates $ sort numberAsStr


result :: Integer -> (Integer -> Bool) -> [Integer]
result maxNumber checkIfPrime = [i | i <- [maxNumber,maxNumber-1..2], isPandigital i && checkIfPrime i]

isPrime :: [Integer] -> Integer -> Bool
isPrime primes number
  | null primes || head primes > number = True
  | mod number (head primes) == 0       = False
  | otherwise                           = isPrime (tail primes) number


main :: IO ()
main = do
  f <- readFile "../primes/primes"
  let primes = [read s :: Integer | s <- lines f]

  print ("Result should be: " ++ show (7652413 :: Integer) ++ ", is: " ++ show (head $ result 7654321 (isPrime primes)))
