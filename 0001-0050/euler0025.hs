-- https://projecteuler.net/problem=25
--
--The Fibonacci sequence is defined by the recurrence relation:
--Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
--Hence the first 12 terms will be:
--F1 = 1
--F2 = 1
--F3 = 2
--F4 = 3
--F5 = 5
--F6 = 8
--F7 = 13
--F8 = 21
--F9 = 34
--F10 = 55
--F11 = 89
--F12 = 144
--The 12th term, F12, is the first term to contain three digits.
--What is the index of the first term in the Fibonacci sequence to contain 1000 digits?

nextFibonacci :: (Integer, Integer) -> (Integer, Integer)
nextFibonacci t = (uncurry (+) t, fst t)

firstFibonacci :: (Integer, Integer)
firstFibonacci = (1, 0)

fibonacciCalculator :: [(Integer, Integer)] -> Integer -> [(Integer, Integer)]
fibonacciCalculator acc maxNumber
  | maxNumber < fst (head acc) = acc
  | otherwise = fibonacciCalculator (nextFibonacci (head acc) : acc) maxNumber

fibonaccis :: Integer -> [Integer]
fibonaccis maxNumber = [fst t | t <- fibonacciCalculator [firstFibonacci] maxNumber ]


main :: IO ()
main = print ("Result should be: " ++ show (4782 :: Integer) ++ ", is: " ++ show (length $ fibonaccis (10^999)))
