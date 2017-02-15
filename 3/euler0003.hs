
-- https://projecteuler.net/problem=3
--
-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?


-- returns biggest prime number (taken from 2nd argument) that divides exactly given number (1st argument)
biggestPF :: Integer -> [Integer] -> Integer
biggestPF number primes
    | mod number biggestPrime == 0 = biggestPrime
    | otherwise                    = biggestPF number $ init primes
    where biggestPrime = last primes

main :: IO ()
main = do
  f <- readFile "../primes/primes"
  let primes = [read s :: Integer | s <- lines f]

  let max1 = 13195
  let biggest1 = biggestPF max1 primes
  print biggest1

  let max2 = 600851475143
  let biggest2 = biggestPF max2 primes
  print biggest2
