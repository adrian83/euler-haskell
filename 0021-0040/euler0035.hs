-- https://projecteuler.net/problem=35
--
-- The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.
-- There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
-- How many circular primes are there below one million?

import Data.List

rotate :: String -> Int -> [String]
rotate _ 0 = []
rotate str count = newStr : (rotate newStr (count - 1))
  where newStr = (tail str) ++ [head str]


isDigitEvenOrZero :: Char -> Bool
isDigitEvenOrZero c = c == '0' || c == '2' || c == '4' || c == '6' || c == '8'


isBuildWithNonEvenDigits :: String -> Bool
isBuildWithNonEvenDigits [] = True
isBuildWithNonEvenDigits str = if isDigitEvenOrZero (head str) then False else isBuildWithNonEvenDigits (tail str)


allPrimes :: [Integer] -> (Integer -> Bool) -> Bool
allPrimes [] _ = True
allPrimes numbers isPrime = if isPrime (head numbers) then allPrimes (tail numbers) isPrime else False


isPrime :: [Integer] -> Integer -> Bool
isPrime [] _ = False
isPrime primes number
  | head primes > number  = False
  | head primes == number = True
  | otherwise             = isPrime (tail primes) number


numberOfCircularPrimes :: [Integer] -> (Integer -> Bool) -> Int
numberOfCircularPrimes [] _ = 0
numberOfCircularPrimes primes isPrime
  | isBuildWithNonEvenDigits primeAsStr = (if allArePrimes then 1 else 0) + numberOfCircularPrimes (tail primes) isPrime
  | otherwise = (if head primes == 2 then 1 else 0) + numberOfCircularPrimes (tail primes) isPrime
  where
    primeAsStr = show (head primes)
    strs = rotate primeAsStr (length primeAsStr)
    numbs = sort $ map (\s -> read s :: Integer) strs
    allArePrimes = allPrimes numbs isPrime


main :: IO ()
main = do
  f <- readFile "../primes/primes"
  let primes = [read s :: Integer | s <- lines f]
  let primesToUse = filter (\n -> n < 1000000) primes

  print ("Expected: " ++ show (55 :: Integer) ++ ", actual: " ++ show (numberOfCircularPrimes primesToUse (isPrime primes)))
