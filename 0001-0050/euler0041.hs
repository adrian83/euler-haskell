import Data.List
-- https://projecteuler.net/problem=41

-- We shall say that an n-digit number is pandigital if it makes use of all
-- the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital
-- and is also prime.
-- What is the largest n-digit pandigital prime that exists?

containsZero :: [Char] -> Bool
containsZero l = elem '0' l

removeDuplicates :: [Char] -> [Char]
removeDuplicates [a] = [a]
removeDuplicates l =
  let
    f = head l
    t = tail l
  in
    if f == head t then removeDuplicates (f : tail t) else f : removeDuplicates t



isPandigital :: Integer -> Bool
isPandigital a =
  let
    str = show a
  in
    if containsZero str then False else (
    let
      l = length str
      uniqueDigits = removeDuplicates $ sort str
    in
      (l == length uniqueDigits) && (last uniqueDigits == head ( show l))
    )




result :: Integer -> [Integer] -> (Integer -> Bool) -> [Integer]
result maxNumber primes checkIfPrime = [i | i <- [maxNumber,maxNumber-1..2], isPandigital i && checkIfPrime i]

isPrime :: [Integer] -> Integer -> Bool
isPrime [] _ = True
isPrime primes e = if e == head primes then isPrime (tail primes) e else (if head primes > e then True else (if mod e (head primes) == 0 then False else isPrime (tail primes) e ))

main :: IO ()
main = do
  f <- readFile "../primes/primes"
  let primes = [read s :: Integer | s <- lines f]


  print(result 4321 primes (isPrime primes))
  print( head $ result 7654321 primes (isPrime primes))
