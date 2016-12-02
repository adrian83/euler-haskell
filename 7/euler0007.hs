-- https://projecteuler.net/problem=7
--
--By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
--What is the 10 001st prime number?

divisable :: (Integral a) => a -> [a] -> Bool
divisable n [] = False
divisable n (x:xs) = if mod n x == 0 then True else divisable n xs

prime :: (Integral a) => a -> a -> [a] -> a
prime c 0 primes = head primes
prime c n primes = if divisable c primes then prime (c+1) n primes else prime (c+1) (n-1) (c:primes)



main = do
  print (prime 3 10000 [2])
