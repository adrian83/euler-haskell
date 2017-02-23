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

calculatePrimeAndSumLen :: Integer -> [Integer] -> [Integer] -> (Integer, Int) -> (Integer -> Bool) -> (Integer, Int)
calculatePrimeAndSumLen _ [] _ tmpSolution _ = tmpSolution
calculatePrimeAndSumLen maxNumber orgPrimes [] tmpSolution checkIfPrime = calculatePrimeAndSumLen maxNumber (tail orgPrimes) (tail orgPrimes) tmpSolution checkIfPrime
calculatePrimeAndSumLen maxNumber orgPrimes primes tmpSolution checkIfPrime =
  if length primes < snd tmpSolution
    then calculatePrimeAndSumLen maxNumber (tail orgPrimes) (trimList maxNumber (tail orgPrimes)) tmpSolution checkIfPrime
    else (
      let
        primesSum = mySum maxNumber 0 primes
      in
        if length primes > snd tmpSolution && primesSum < maxNumber && checkIfPrime primesSum
          then calculatePrimeAndSumLen maxNumber orgPrimes (init primes) (primesSum, length primes) checkIfPrime
          else calculatePrimeAndSumLen maxNumber orgPrimes (init primes) tmpSolution checkIfPrime
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

result :: Integer -> [Integer] -> (Integer, Int)
result number primes = calculatePrimeAndSumLen number primes (filter (<number) primes) (0,0) (isPrime primes)

main :: IO ()
main = do
  f <- readFile "../primes/primes"
  let primes = [read s :: Integer | s <- lines f]

  print ("Result should be: " ++ show (953 :: Integer) ++ ", is: " ++ show (fst $ result 1000 primes))
  print ("Result should be: " ++ show (997651 :: Integer) ++ ", is: " ++ show (fst $ result 1000000 primes ))
