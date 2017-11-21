-- https://projecteuler.net/problem=37
--
-- The number 3797 has an interesting property. Being prime itself, it is possible to continuously
-- remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly
-- we can work from right to left: 3797, 379, 37, and 3.
-- Find the sum of the only eleven primes that are both truncatable from left to right and right to left.
-- NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

import Data.List

isPrime :: [Integer] -> Integer -> Bool
isPrime [] _ = False
isPrime primes number
  | number > 7 && (mod number 2 == 0 || mod number 3 == 0 || mod number 5 == 0 || mod number 7 == 0) = False
  | head primes == number = True
  | head primes > number  = False
  | otherwise             = isPrime (tail primes) number

removeTrailingZeros :: String -> String
removeTrailingZeros "" = ""
removeTrailingZeros str
  | head str == '0' = removeTrailingZeros (tail str)
  | otherwise       = str

trimR :: Integer -> [Integer]
trimR number
  | number < 10 = []
  | otherwise   = smaller : trimR smaller
    where smaller = read (init (show number)) :: Integer

trimL :: Integer -> [Integer]
trimL number
  | number < 10 = []
  | str == "" = []
  | otherwise = (read str :: Integer) : trimL (read str :: Integer)
    where str = removeTrailingZeros $ tail (show number)

numbersFromBase :: Integer -> [Integer]
numbersFromBase number = number : (trimR number ++ trimL number)

truncatablePrimes :: [Integer] -> [Integer]
truncatablePrimes primes = [i | i <- primes, all (isPrime primes) (sort $ numbersFromBase i) ]

sumOfTruncatablePrimes :: [Integer] -> Integer
sumOfTruncatablePrimes primes = sum $ filter (>10) (truncatablePrimes primes)

main :: IO ()
main = do
  f <- readFile "../primes/primes"
  let primes = [read s :: Integer | s <- lines f]

  print ("Expected: " ++ show (748317 :: Integer) ++ ", actual: " ++ show (sumOfTruncatablePrimes primes))
