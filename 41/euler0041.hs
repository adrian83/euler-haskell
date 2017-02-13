import Data.List
-- https://projecteuler.net/problem=41

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



isDivisable :: Integer -> [Integer] -> Bool
isDivisable number [] = False
isDivisable number numbers =  if mod number (head numbers) == 0 then True else isDivisable number (tail numbers)

primes :: Integer -> Integer -> [Integer] -> [Integer]
primes maxNumb currentNumber acc = if maxNumb == currentNumber then acc else (
    if isDivisable currentNumber acc then primes maxNumb (currentNumber+1) acc else primes maxNumb (currentNumber+1) (acc++[currentNumber])
  )

result :: Integer -> [Integer]
result maxNumber = [i | i <- reverse (siev [2,3..maxNumber] 2 (floor $ sqrt $ fromIntegral maxNumber)), isPandigital i ]

isPrime2 :: Integer -> Bool
isPrime2 a = if mod a 2 == 0 || mod a 3 == 0 || isDivisable a (2:[3,5..(floor $ sqrt $ fromIntegral a)]) then False else True

result2 :: Integer -> [Integer]
result2 maxNumber =
  let
    pr = primes maxNumber 3 [2]
  in
    [i | i <- [maxNumber, maxNumber-1..11], isPandigital i]-- && isPrime2 i]

remoDivisors :: [Integer] -> Integer -> [Integer]
remoDivisors tab a = [i | i <- tab, a == i || mod i a /= 0]

siev :: [Integer] -> Integer -> Integer -> [Integer]
siev tab currentIter maxIter = if currentIter == maxIter then tab else (
  siev (remoDivisors tab currentIter) (currentIter+1) maxIter
  )



main :: IO ()
main = do
  print( sort "12434")
  print( removeDuplicates $ sort "12434")
  print( isPandigital 1234)
  print( isPandigital 12346)
  print( isPandigital 1243)
  --print( length $ primes 654321 3 [2])
  --print( [i | i <- [9999999, 9999998..11], isPandigital i])
  --print( length $ primes 7654321 3 [2]  )
  --print( length (siev [2,3..54321] 2 (floor $ sqrt $ fromIntegral 54321)))
  print( head $ result 7654321)

  print( removeDuplicates $ sort $ show 113 )
  print( last (removeDuplicates $ sort $ show 113) )
