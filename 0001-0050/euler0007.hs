-- https://projecteuler.net/problem=7
--
-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
-- What is the 10 001st prime number?

prime :: [Integer] -> Int -> Integer
prime primes e = last $ take e primes


main :: IO ()
main = do
  f <- readFile "../primes/primes"
  let primes = [read s :: Integer | s <- lines f]

  print ("Result should be: " ++ show (13 :: Integer) ++ ", is: " ++ show (prime primes 6))
  print ("Result should be: " ++ show (104743 :: Integer) ++ ", is: " ++ show (prime primes 10001))
