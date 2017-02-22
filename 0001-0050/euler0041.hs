-- https://projecteuler.net/problem=41

-- We shall say that an n-digit number is pandigital if it makes use of all
-- the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital
-- and is also prime.
-- What is the largest n-digit pandigital prime that exists?

import Data.List

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [a] = [a]
removeDuplicates l =
  let
    f = head l
    t = tail l
  in
    if f == head t then removeDuplicates (f : tail t) else f : removeDuplicates t


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
isPrime [] _ = True
isPrime primes e = if e == head primes then isPrime (tail primes) e else (if head primes > e then True else (if mod e (head primes) == 0 then False else isPrime (tail primes) e ))

main :: IO ()
main = do
  f <- readFile "../primes/primes"
  let primes = [read s :: Integer | s <- lines f]

  print ("Result should be: " ++ show (7652413 :: Integer) ++ ", is: " ++ show (head $ result 7654321 (isPrime primes)))
