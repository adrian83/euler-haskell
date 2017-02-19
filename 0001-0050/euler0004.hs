-- https://projecteuler.net/problem=4
--
-- A palindromic number reads the same both ways. The largest palindrome made
-- from the product of two 2-digit numbers is 9009 = 91 × 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.


isPalindrom :: (Eq a) => [a] -> Bool
isPalindrom [] = True
isPalindrom [_] = True
isPalindrom (x:xs) = (x == last xs) && isPalindrom (init xs)

largest :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer
largest maxNumb1 maxNumb2 minNumb1 minNumb2 currentNumb1 currentNumb2 currentBiggest
  | maxNumb1 == currentNumb1 && maxNumb2 == currentNumb2 = currentBiggest
  | maxNumb2 < currentNumb2 = largest maxNumb1 maxNumb2 minNumb1 minNumb2 (currentNumb1+1) minNumb2 currentBiggest
  | otherwise =
    let
      mult = currentNumb2 * currentNumb1
    in
      if mult > currentBiggest && isPalindrom (show mult)
        then largest maxNumb1 maxNumb2 minNumb1 minNumb2 currentNumb1 (currentNumb2+1) mult
        else largest maxNumb1 maxNumb2 minNumb1 minNumb2 currentNumb1 (currentNumb2+1) currentBiggest

largestBetween100And999 :: Integer -> Integer -> Integer -> Integer
largestBetween100And999 = largest 999 999 100 100

largestBetween10And99 :: Integer -> Integer -> Integer -> Integer
largestBetween10And99 = largest 99 99 10 10

main :: IO ()
main = do
  print ("Result should be: " ++ show (9009 :: Integer) ++ ", is: " ++ show (largestBetween10And99 10 10 0))
  print ("Result should be: " ++ show (906609 :: Integer) ++ ", is: " ++ show (largestBetween100And999 100 100 0))
