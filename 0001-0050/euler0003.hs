-- https://projecteuler.net/problem=3
--
-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?


-- returns biggest prime number (taken from 2nd argument) that divides exactly given number (1st argument)
biggestPF :: Integer -> [Integer] -> Integer
biggestPF number primes
    | mod number biggestPrime == 0 = biggestPrime
    | otherwise                    = biggestPF number $ tail primes
    where biggestPrime = head primes

main :: IO ()
main = do
  f <- readFile "../primes/primes"
  let primes = reverse [read s :: Integer | s <- lines f]

  let max1 = 13195
  let biggest1 = biggestPF max1 primes
  print ("Result should be: " ++ show (29 :: Integer) ++ ", is: " ++ show biggest1)

  let max2 = 600851475143
  let biggest2 = biggestPF max2 primes
  print ("Result should be: " ++ show (6857 :: Integer) ++ ", is: " ++ show biggest2)
