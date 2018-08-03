-- https://projecteuler.net/problem=27
--
--

minA :: Integer
minA = -999
maxA :: Integer
maxA = 999

minB :: Integer
minB = -1000
maxB :: Integer
maxB = 1000

minN :: Integer
minN = 0
maxN :: Integer
maxN = 200

data Solution = Solution {
    len :: Integer,
    a :: Integer,
    b :: Integer
  } deriving (Show, Eq)

isPrime :: [Integer] -> Integer -> Bool
isPrime [] _ = False
isPrime primes number
  | number < 2 = False
  | number > 7 && (mod number 2 == 0 || mod number 3 == 0 || mod number 5 == 0 || mod number 7 == 0 || mod number 11 == 0 || mod number 13 == 0) = False
  | head primes == number = True
  | head primes > number  = False
  | otherwise             = isPrime (tail primes) number

isAlmostPrime :: [Integer] -> Integer -> Bool
isAlmostPrime primes number
  | number <= -2 = isPrime primes (-number)
  | number == -1 || number == 0 || number == 1 = True
  | otherwise = isPrime primes number

calculate :: Integer -> Integer -> Integer -> Integer
calculate a b n = (n^2) + (a * n) + b

simplePrimeTest :: Integer -> Bool
simplePrimeTest n
  | n < 2 = False
  | n > 41 && (mod n 2 == 0 || mod n 3 == 0 || mod n 5 == 0 || mod n 7 == 0 || mod n 11 == 0 || mod n 13 == 0 || mod n 17 == 0 || mod n 19 == 0 || mod n 23 == 0 || mod n 29 == 0 || mod n 31 == 0 || mod n 37 == 0 || mod n 41 == 0) = False
  | n <= 41 && (n == 2 || n == 3 || n == 5 || n == 7 || n == 11 || n == 13 || n ==17 || n == 19 || n == 23 || n == 29 || n == 31 || n == 39 || n == 41) = True
  | n <= 41 = False
  | otherwise = True



divide :: [Integer] -> (Integer -> Bool) -> [Integer] -> [[Integer]]
divide [] _ acc = [acc]
divide candidates maybePrime acc =
  if maybePrime (head candidates)
    then divide (tail candidates) maybePrime ((head candidates) : acc)
    else
      if length acc > 50
        then acc : (divide (tail candidates) maybePrime [])
        else divide (tail candidates) maybePrime []

primesInRow :: [Integer] -> (Integer -> Bool) -> Integer -> Integer
primesInRow [] _ acc = acc
primesInRow numbers isPrime acc =
  if isPrime (head numbers)
    then primesInRow (tail numbers) isPrime (acc + 1)
    else max acc (primesInRow (tail numbers) isPrime 0)


longest :: [[Integer]] -> (Integer -> Bool) -> Integer -> Integer
longest [] _ cur = cur
longest [a] isPrime cur = if toInteger (length a) > cur then max cur (primesInRow a isPrime 0) else cur
longest lol isPrime cur
    | lngst > current = lngst
    | otherwise = current
    where
      lngst = longest (tail lol) isPrime cur
      current = if toInteger (length (head lol)) > cur then max cur (primesInRow (head lol) isPrime 0) else cur

result :: Integer -> Integer -> Solution -> (Integer -> Bool) -> (Integer -> Bool) -> Solution
result a b best isPrime testB
  | a >= maxA && b >= maxB = best
  | b >= maxB = result (a+1) minB best isPrime testB
  | not (testB b) = result a (b+1) best isPrime testB
  | mod a 2 == 0 = result (a+1) b best isPrime testB
  | lgst > len best = result a (b+1) (Solution {len=lgst, a=a, b=b}) isPrime testB
  | otherwise = result a (b+1) best isPrime testB
  where
    numbers = [calculate a b n | n <- [minN, minN+1..maxN]]
    possiblePrimes = divide numbers simplePrimeTest []
    lgst = longest possiblePrimes isPrime (len best)

main :: IO ()
main = do
  let maxPrime = (maxN^2) + (maxA*maxN) + maxB
  f <- readFile "../primes/primes"
  let primes = filter (\n -> n <= maxPrime) [read s :: Integer | s <- lines f]

  print(result minA minB Solution {len=0, a=0, b=0} (isPrime primes) (isAlmostPrime primes))
