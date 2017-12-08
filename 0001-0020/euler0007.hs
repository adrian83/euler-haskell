-- https://projecteuler.net/problem=7
--
-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
-- What is the 10 001st prime number?

primeByIndex :: [Integer] -> Int -> Integer
primeByIndex primes e = primes !! (e - 1)

main :: IO ()
main = do
  f <- readFile "../primes/primes"
  let primes = [read s :: Integer | s <- lines f]

  --print ("Expected: " ++ show (13 :: Integer) ++ ", actual: " ++ show (primeByIndex primes 6))
  print ("Expected: " ++ show (104743 :: Integer) ++ ", actual: " ++ show (primeByIndex primes 10001))
