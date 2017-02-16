import Data.List

-- returns True if given number is divisable by any number from given list, False otherwise
divisable :: (Integral a) => a -> [a] -> Bool
divisable 0 _ = False
divisable n [] = False
divisable n (x:xs) = if mod n x == 0 then True else divisable n xs

calculatePrimes :: Integer -> Integer -> [Integer] -> [Integer]
calculatePrimes maxNumber currentNumber smallerPrimes
  | maxNumber <= currentNumber = smallerPrimes
  | divisable currentNumber smallerPrimes = calculatePrimes maxNumber (currentNumber+1) smallerPrimes
  | otherwise = calculatePrimes maxNumber (currentNumber+1) (smallerPrimes ++ [currentNumber])

toListOfIntegers :: [String] -> [Integer]
toListOfIntegers strings = [read s :: Integer | s <- strings]

main :: IO ()
main = do
  f   <- readFile "./primes"
  let smallerPrimes = toListOfIntegers (lines f)
  print ( show (length smallerPrimes) ++ " prime numbers already in file")
  let current = if null smallerPrimes then 2 else last smallerPrimes
  let maxNumber = 1010000
  let primes = calculatePrimes maxNumber current smallerPrimes
  let primesstr = intercalate "\n" [show i | i <- primes]
  --appendFile "./primes" primesstr
  writeFile "./primes" primesstr
