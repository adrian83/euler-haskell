-- https://projecteuler.net/problem=2
--
-- Each new term in the Fibonacci sequence is generated by adding the previous
-- two terms. By starting with 1 and 2, the first 10 terms will be:
-- 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
-- By considering the terms in the Fibonacci sequence whose values do not
-- exceed four million, find the sum of the even-valued terms.

nextFibonacci :: (Integer, Integer) -> (Integer, Integer)
nextFibonacci t = (uncurry (+) t, fst t)

firstFibonacci :: (Integer, Integer)
firstFibonacci = (1, 0)

fibonacciCalculator :: [(Integer, Integer)] -> Integer -> [(Integer, Integer)]
fibonacciCalculator acc maxNumber
  | maxNumber < fst (head acc) = tail acc
  | otherwise = fibonacciCalculator (nextFibonacci (head acc) : acc) maxNumber

fibonaccis :: Integer -> [Integer]
fibonaccis maxNumber = [fst t | t <- fibonacciCalculator [firstFibonacci] maxNumber ]

evenFibonaccis :: Integer -> [Integer]
evenFibonaccis maxNumber = filter even (fibonaccis maxNumber)

main :: IO ()
main = do
  --let res1 = sum $ evenFibonaccis 100
  --print ("Expected: " ++ show (44 :: Integer) ++ ", actual: " ++ show res1)
  let res2 = sum $ evenFibonaccis 4000000
  print ("Expected: " ++ show (4613732 :: Integer) ++ ", actual: " ++ show res2)
