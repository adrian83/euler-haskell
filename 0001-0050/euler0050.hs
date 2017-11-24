-- https://projecteuler.net/problem=50

-- The prime 41, can be written as the sum of six consecutive primes:
--     41 = 2 + 3 + 5 + 7 + 11 + 13
-- This is the longest sum of consecutive primes that adds to a prime below one-hundred.
-- The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.
-- Which prime, below one-million, can be written as the sum of the most consecutive primes?

import Data.List
import Data.Ord
import Data.Either

data Solution = Solution {
    number :: Integer,
    sumLength :: Integer
  } deriving (Show, Eq)

instance Ord Solution where
  compare s1 s2
    | sumLength s1 > sumLength s2 = LT
    | sumLength s1 < sumLength s2 = GT
    | otherwise = EQ

buildFromConsecutivePrimes :: [Integer] -> Integer -> Either Bool Integer
buildFromConsecutivePrimes [] _ = Right 0
buildFromConsecutivePrimes _ 0 = Right 0
buildFromConsecutivePrimes primes number
  | head primes > number = Left False
  | head primes == number = Right 1
  | otherwise = case buildFromConsecutivePrimes (tail primes) (number - head primes) of
                  Right n -> Right (n + 1)
                  Left b -> Left b

consecutivePrimesSums :: [Integer] -> Integer -> [Integer]
consecutivePrimesSums [] _ = []
consecutivePrimesSums primes number
  | head primes > number = []
  | otherwise = case buildFromConsecutivePrimes primes number of
                  Right n -> n : consecutivePrimesSums (tail primes) number
                  Left b -> consecutivePrimesSums (tail primes) number

longestConsecutivePrimesSums :: [Integer] -> Integer -> Solution
longestConsecutivePrimesSums primes number = Solution { number=number, sumLength=(maximum (consecutivePrimesSums primes number))}

solutions :: [Integer] -> Integer -> Integer
solutions primes maxNumber = number $ head $ sort [longestConsecutivePrimesSums primes n | n <- (takeWhile (<= maxNumber) primes)]


main :: IO ()
main = do
  f <- readFile "../primes/primes"
  let primes = [read s :: Integer | s <- lines f]

  print(take 10 primes)
  print(consecutivePrimesSums primes 953)

  print ("Result should be: " ++ show (953 :: Integer) ++ ", is: " ++ show (solutions primes 1000))
  print ("Result should be: " ++ show (997651 :: Integer) ++ ", is: " ++ show (solutions primes 1000000))
