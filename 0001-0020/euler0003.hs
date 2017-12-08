-- https://projecteuler.net/problem=3
--
-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?


-- returns biggest prime number (taken from 2nd argument) that divides exactly given number (1st argument)
biggestPF :: [Integer] -> Integer -> Integer
biggestPF primes number
    | mod number biggestPrime == 0 = biggestPrime
    | otherwise                    = biggestPF (tail primes) number 
    where biggestPrime = head primes

main :: IO ()
main = do
  f <- readFile "../primes/primes"
  let primes = reverse [read s :: Integer | s <- lines f]

  let pfCalculator = biggestPF primes

  --let biggest1 = pfCalculator 13195
  --print ("Expected: " ++ show (29 :: Integer) ++ ", actual: " ++ show biggest1)

  let biggest2 = pfCalculator 600851475143
  print ("Expected: " ++ show (6857 :: Integer) ++ ", actual: " ++ show biggest2)
