-- https://projecteuler.net/problem=77
--
-- It is possible to write ten as the sum of primes in exactly five different ways:
--   7 + 3
--   5 + 5
--   5 + 3 + 2
--   3 + 3 + 2 + 2
--   2 + 2 + 2 + 2 + 2
-- What is the first value which can be written as the sum of primes in over five thousand different ways?


solutions :: Integer -> [Integer] -> Integer -> Integer
solutions _ [] acc = acc
solutions left primes acc
  | head primes > left = solutions left (tail primes) acc
  | head primes < left = solutions (left-head primes) primes acc + solutions left (tail primes) acc
  | otherwise = (acc+1) + solutions left (tail primes) acc

solve :: [Integer] -> Integer -> Integer -> Integer

solve allPrimes maxSolutions current = if last allPrimes < current then -1 else (
  let
    primes = reverse $ takeWhile (<current) allPrimes
    numberOfSolutions = solutions current primes 0
  in
    if numberOfSolutions >= maxSolutions then current else solve allPrimes maxSolutions (current+1)
  )


main :: IO ()
main = do
  f <- readFile "../primes/primes"
  let primes = [read s :: Integer | s <- lines f]

  print ("Result should be: " ++ show (71 :: Integer) ++ ", is: " ++ show (solve primes 5000 3))
