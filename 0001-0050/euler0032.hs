import Data.List

-- We shall say that an n-digit number is pandigital if it makes use of all the
-- digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.
-- The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.
-- Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
-- HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.


containsZero :: [Char] -> Bool
containsZero l = elem '0' l

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [a] = [a]
removeDuplicates l =
  let
    f = head l
    t = tail l
  in
    if f == head t then removeDuplicates (f : tail t) else f : removeDuplicates t



isPandigital :: [Char] -> Bool
isPandigital str =
    if containsZero str then False else (
    let
      l = length str
      uniqueDigits = removeDuplicates $ sort str
    in
      (l == length uniqueDigits) && (last uniqueDigits == head ( show l))
    )

bla2 :: Integer -> Integer -> [(Integer,Integer,Integer)]
bla2 maxNumber a =
  let
    maxb = quot maxNumber a
  in
    [(a,b,a*b) | b <- [maxb,maxb-1..a], length ((show a) ++ (show b) ++ (show (a*b))) == 9 && isPandigital ((show a) ++ (show b) ++ (show (a*b))) ]
-- && (read ((show a) ++ (show b) ++ (show (a*b))) :: Integer) >= 123

bla :: Integer -> Integer -> [(Integer,Integer,Integer)] -> [(Integer,Integer,Integer)]
bla maxNumber currentNumber acc
  | maxNumber == currentNumber = acc
  | otherwise = bla maxNumber (currentNumber+1) (acc ++ bla2 maxNumber currentNumber)

-- = [bla2 i | i <- [2,3..4321]]


main :: IO ()
main = do
  --print(pows 2 100)
  --print(length $ allNumbs 100)
  let prods = [c | (_,_,c) <- bla 99999 1 []]
  print( prods )
  let noDups = removeDuplicates prods
  print( sum noDups )
