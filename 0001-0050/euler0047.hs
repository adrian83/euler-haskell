-- https://projecteuler.net/problem=47

import Data.List

wielokrotnosci :: Integer -> Integer -> [Integer]
wielokrotnosci number maxx = [number ^ i | i <- [1,2.. (floor $ sqrt $ fromIntegral maxx)], mod maxx (number ^ i) == 0]

divisableByPrimes :: [Integer] -> Integer -> Integer -> Bool
divisableByPrimes [] _ _ = False
divisableByPrimes _ n 0 = n == 1
divisableByPrimes primes number times =
  let prime = head primes
  in if number < prime
    then False
    else
      if mod number (prime) /= 0
        then divisableByPrimes (tail primes) number times
        else
          let wiel = wielokrotnosci (head primes) number
              newNumbs = [quot number i | i <- wiel, mod number i == 0]
              maybeSol = find (\a -> a == True) [ divisableByPrimes (tail primes) i (times-1) | i <- newNumbs]
          in
            case maybeSol of
              Just _ -> True
              Nothing -> divisableByPrimes (tail primes) number times






result :: [Integer] -> Integer -> Int -> Integer -> [Integer] -> [Integer]
result primes cur len divs acc =
  if length acc == len
    then acc
    else
      let divisable = divisableByPrimes primes cur divs
      in if divisable
        then result primes (cur+1) len divs (cur : acc)
        else result primes (cur+1) len divs []


main :: IO ()
main = do
  f <- readFile "../primes/primes"
  let primes = [read s :: Integer | s <- lines f]

  print (result (take 800 primes) 1 4 4 [])
