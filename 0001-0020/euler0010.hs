-- https://projecteuler.net/problem=10
--
-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
-- Find the sum of all the primes below two million.

sumAllBelow :: [Integer] -> Integer -> Integer
sumAllBelow numbers maxx = sum $ filter (<maxx) numbers

main :: IO ()
main = do
  f <- readFile "../primes/primes"
  let primes = reverse [read s :: Integer | s <- lines f]
  
  print ("Expected: " ++ show (142913828922 :: Integer) ++ ", actual: " ++ show (sumAllBelow primes 2000000))
