-- https://projecteuler.net/problem=35
--
-- The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.
-- There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
-- How many circular primes are there below one million?

import Data.List

isPrime :: [Integer] -> Integer -> Bool
isPrime [] _ = False
isPrime primes number
  | number > 7 && (mod number 2 == 0 || mod number 3 == 0 || mod number 5 == 0 || mod number 7 == 0) = False
  | head primes == number = True
  | head primes > number  = False
  | otherwise             = isPrime (tail primes) number

rot :: String -> Int -> Int -> [String]
rot _ _ 0 = []
rot str len count = (take len str) : rot (tail str) len (count-1)

permutation :: Integer -> [Integer]
permutation number = map (\n -> read n :: Integer) (rot (cycle (show number)) rotations rotations)
  where rotations = length (show number)

rotatedPrimes :: [Integer] -> [[Integer]]
rotatedPrimes primes = map permutation primes

allPrimes :: (Integer -> Bool) -> [Integer] -> Bool
allPrimes prime numbers = all prime (sort numbers)

shouldRotate :: Integer -> Bool
shouldRotate number = if number == 2
  then True
  else not $ any (\ch -> ch `elem` ['2','4','6','8','0']) (show number)

circularPrimes :: (Integer -> Bool) -> [Integer] -> [Integer]
circularPrimes prime primes = [head rotated | rotated <- rotatedPrimes (filter shouldRotate primes), allPrimes prime rotated ]

--numberOfCircularPrimes :: (Integer -> Bool) -> [Integer] -> Int
--numberOfCircularPrimes prime primes = length $ circularPrimes prime primes

numberOfCircularPrimes :: [Integer] -> Int
numberOfCircularPrimes primes = length $ circularPrimes (isPrime primes) primes

main :: IO ()
main = do
  f <- readFile "../primes/primes"
  let primes = [read s :: Integer | s <- lines f]

  --print ("Expected: " ++ show (13 :: Integer) ++ ", actual: " ++ show (numberOfCircularPrimes (filter (<100) primes)))
  print ("Expected: " ++ show (55 :: Integer) ++ ", actual: " ++ show (numberOfCircularPrimes (filter (<1000000) primes)))
