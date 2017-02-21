-- https://projecteuler.net/problem=35
--
-- The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.
-- There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
-- How many circular primes are there below one million?

digits :: Integer -> [Integer]
digits 0 = []
digits n = (mod n 10) : digits (quot n 10)

toNumber :: Integer -> [Integer] -> Integer
toNumber _ [] = 0
toNumber wyk digits  = ((head digits)*10^wyk) + toNumber (wyk+1) (tail digits)

rotate :: [Integer] -> Int -> [[Integer]]
rotate _ 0 = []
rotate l rotCount =
  let
    f = (tail l ++ [head l])
  in
    f : rotate f (rotCount-1)



isPrime :: [Integer] -> Integer -> Bool
isPrime primes number = number `elem` primes


perf :: Integer -> [Integer]
perf numb =
  let
    d = digits numb
    toNumber2 = toNumber 0
  in
    map (toNumber2) (rotate d (length d))



result :: [Integer] -> Int
result primes = length [i | i <- primes, all (isPrime primes) (perf i) ]

main :: IO ()
main = do
  f <- readFile "../primes/primes"
  let primes = [read s :: Integer | s <- lines f]

  print ("Result should be: " ++ show (13 :: Integer) ++ ", is: " ++ show (result (filter (<100) primes)))
  print ("Result should be: " ++ show (55 :: Integer) ++ ", is: " ++ show (result primes))
