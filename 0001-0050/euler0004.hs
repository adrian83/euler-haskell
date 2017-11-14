-- https://projecteuler.net/problem=4
--
-- A palindromic number reads the same both ways. The largest palindrome made
-- from the product of two 2-digit numbers is 9009 = 91 × 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.

isPalindrom :: (Eq a) => [a] -> Bool
isPalindrom [] = True
isPalindrom [_] = True
isPalindrom (x:xs) = (x == last xs) && isPalindrom (init xs)

pairsWithDigits :: Integer -> [(Integer, Integer)]
pairsWithDigits digits =
  let
    minNumb = 10 ^ (digits - 1)
    maxNumb = (10 ^ digits) - 1
  in [(a, b) | a <- [minNumb..maxNumb], b <- [a..maxNumb]]

maxPalindrome :: [(Integer, Integer)] -> Integer
maxPalindrome pairs = maximum (filter (\numb -> isPalindrom (show numb)) (map (\t -> fst t * snd t) pairs))

main :: IO ()
main = do
  --print ("Expected: " ++ show (9009 :: Integer) ++ ", actual: " ++ show (maxPalindrome $ pairsWithDigits 2))
  print ("Expected: " ++ show (906609 :: Integer) ++ ", actual: " ++ show (maxPalindrome $ pairsWithDigits 3))
