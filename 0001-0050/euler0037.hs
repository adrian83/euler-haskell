-- https://projecteuler.net/problem=37
--
-- The number 3797 has an interesting property. Being prime itself, it is possible to continuously
-- remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly
-- we can work from right to left: 3797, 379, 37, and 3.
-- Find the sum of the only eleven primes that are both truncatable from left to right and right to left.
-- NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

import Data.List

isPrime :: [Integer] -> Integer -> Bool
isPrime primes number = number `elem` primes

trimR :: Integer -> [Integer]
trimR number
  | number == 0 || smallerNumber == 0 = []
  | otherwise = smallerNumber : trimR smallerNumber
  where
    numberLen = length (show number)
    smallerNumber = mod number (10^ (numberLen-1))

trimL :: Integer -> [Integer]
trimL number
  | number == 0 || smallerNumber == 0 = []
  | otherwise = smallerNumber : trimL smallerNumber
  where
    smallerNumber = quot number 10

perf :: Integer -> [Integer]
perf number = (number : trimR number) ++ trimL number

specialPrimes :: [Integer] -> [Integer]
specialPrimes primes = [i | i <- primes, all (isPrime primes) (sort $ perf i) ]

result :: [Integer] -> Integer
result primes = sum $ filter (>10) (specialPrimes primes)

main :: IO ()
main = do
  f <- readFile "../primes/primes"
  let primes = [read s :: Integer | s <- lines f]

  print ("Result should be: " ++ show (748317 :: Integer) ++ ", is: " ++ show (result primes))
