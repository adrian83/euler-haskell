-- https://projecteuler.net/problem=7
--
-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
-- What is the 10 001st prime number?

divisable :: Integer -> [Integer] -> Bool
divisable _ [] = False
divisable n (x:xs) = if mod n x == 0 then True else divisable n xs

prime :: Integer -> Integer -> [Integer] -> Integer
prime _ 0 primes = head primes
prime c n primes = if divisable c primes then prime (c+1) n primes else prime (c+1) (n-1) (c:primes)


main :: IO ()
main = do
  print ( prime 3 5 [2] )
  print ( prime 3 10000 [2] )
