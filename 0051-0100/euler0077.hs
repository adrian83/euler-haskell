-- https://projecteuler.net/problem=77
--
-- It is possible to write ten as the sum of primes in exactly five different ways:
--   7 + 3
--   5 + 5
--   5 + 3 + 2
--   3 + 3 + 2 + 2
--   2 + 2 + 2 + 2 + 2
-- What is the first value which can be written as the sum of primes in over five thousand different ways?

sumCount :: [Integer] -> Integer -> Integer
sumCount [] _ = 0
sumCount primes left
  | head primes > left = 0
  | head primes < left = sumCount primes (left-head primes) + sumCount (tail primes) left
  | otherwise = 1 + sumCount (tail primes) left

sumOfPrimesOfXDifferentWays :: [Integer] -> Integer -> [Integer]
sumOfPrimesOfXDifferentWays primes solutionsNumb = filter (\n -> (sumCount primes n) > solutionsNumb) [1,2..]

main :: IO ()
main = do
  f <- readFile "../primes/primes"
  let primes = [read s :: Integer | s <- lines f]

  print ("Expected: " ++ show (71 :: Integer) ++ ", actual: " ++ show (head $ sumOfPrimesOfXDifferentWays primes 5000))
