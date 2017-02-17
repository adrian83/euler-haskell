-- https://projecteuler.net/problem=50

-- The prime 41, can be written as the sum of six consecutive primes:
--     41 = 2 + 3 + 5 + 7 + 11 + 13
-- This is the longest sum of consecutive primes that adds to a prime below one-hundred.
-- The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.
-- Which prime, below one-million, can be written as the sum of the most consecutive primes?

mySum :: Integer -> Integer -> [Integer] -> Integer
mySum maxNumber tmpResult elems
  | null elems || tmpResult > maxNumber = tmpResult
  | otherwise                           = mySum maxNumber (tmpResult + head elems) (tail elems)

result :: Integer -> [Integer] -> [Integer] -> (Integer, Int) -> (Integer -> Bool) -> (Integer, Int)
result _ [] _ tmpSolution _ = tmpSolution
result maxNumber orgPrimes [] tmpSolution checkIfPrime = result maxNumber (tail orgPrimes) (tail orgPrimes) tmpSolution checkIfPrime
result maxNumber orgPrimes primes tmpSolution checkIfPrime =
  if length primes < snd tmpSolution
    then result maxNumber (tail orgPrimes) (trimList maxNumber (tail orgPrimes)) tmpSolution checkIfPrime
    else (
      let
        primesSum = mySum maxNumber 0 primes
      in
        if length primes > snd tmpSolution && primesSum < maxNumber && checkIfPrime primesSum
          then result maxNumber orgPrimes (init primes) (primesSum, length primes) checkIfPrime
          else result maxNumber orgPrimes (init primes) tmpSolution checkIfPrime
    )

trimList :: Integer -> [Integer] -> [Integer]
trimList needed numbers
  | null numbers || needed < 0 = []
  | otherwise                  = head numbers : trimList (needed - head numbers) (tail numbers)

isPrime :: [Integer] -> Integer -> Bool
isPrime primes number
  | null primes || number < head primes = True
  | number == head primes               = isPrime (tail primes) number
  | mod number (head primes) == 0       = False
  | otherwise                           = isPrime (tail primes) number

main :: IO ()
main = do
  f <- readFile "../primes/primes"
  let primes = [read s :: Integer | s <- lines f]

  let numberIsPrime = isPrime primes

  print (result 100 primes (trimList 100 primes) (0,0) numberIsPrime)
  print (result 1000 primes (trimList 1000 primes) (0,0) numberIsPrime)
  print (result 1000000 primes (trimList 1000000 primes) (0,0) numberIsPrime)
