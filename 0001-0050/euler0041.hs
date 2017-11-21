-- https://projecteuler.net/problem=41

-- We shall say that an n-digit number is pandigital if it makes use of all
-- the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital
-- and is also prime.
-- What is the largest n-digit pandigital prime that exists?

import Data.List

pandigital :: Integer -> Bool
pandigital number = case length sorted of
          0 -> False
          1 -> sorted == "1"
          2 -> sorted == "12"
          3 -> sorted == "123"
          4 -> sorted == "1234"
          5 -> sorted == "12345"
          6 -> sorted == "123456"
          7 -> sorted == "1234567"
          8 -> sorted == "12345678"
          9 -> sorted == "123456789"
          _ -> False
          where sorted = sort $ show number

biggestPandigital :: Integer -> (Integer -> Bool) -> Integer
biggestPandigital maxNumber prime = head [i | i <- [maxNumber,maxNumber-1..2], pandigital i && prime i]

isPrime :: [Integer] -> Integer -> Bool
isPrime [] _ = False
isPrime primes number
  | head primes > number = False
  | head primes == number = True
  | otherwise  = isPrime (tail primes) number

main :: IO ()
main = do
  f <- readFile "../primes/primes"
  let primes = [read s :: Integer | s <- lines f]

  print ("Expected: " ++ show (7652413 :: Integer) ++ ", actual: " ++ show (biggestPandigital 7654321 (isPrime primes)))
