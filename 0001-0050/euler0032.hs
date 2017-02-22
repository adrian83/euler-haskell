-- https://projecteuler.net/problem=32
--
-- We shall say that an n-digit number is pandigital if it makes use of all the
-- digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.
-- The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.
-- Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
-- HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.


import Data.List

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [a] = [a]
removeDuplicates l =
  let
    f = head l
    t = tail l
  in
    if f == head t then removeDuplicates (f : tail t) else f : removeDuplicates t


isPandigital :: String -> Bool
isPandigital numberAsStr
  | '0' `elem` numberAsStr = False
  | (orgLen == length uniqueDigits) && (last uniqueDigits == head (show orgLen)) = True
  | otherwise = False
  where
    orgLen = length numberAsStr
    uniqueDigits = removeDuplicates $ sort numberAsStr



calc :: Integer -> Integer -> Integer -> [(Integer,Integer,Integer)] -> [(Integer,Integer,Integer)]
calc maxNumber number1 number2 acc
  | number1 > maxNumber                                       = acc
  | number1 >= number2                                        = calc maxNumber number1 (number1+1) acc
  | prod > maxNumber                                          = calc maxNumber (number1+1) (number1+2) acc
  -- | length str == length (show maxNumber) && isPandigital str = calc maxNumber number1 (number2+1) ((number1, number2, prod):acc)
  | isPandigital str = calc maxNumber number1 (number2+1) ((number1, number2, prod):acc)
  | otherwise                                                 = calc maxNumber number1 (number2+1) acc
  where
    prod = number1 * number2
    str = show number1 ++ show number2 ++ show prod




main :: IO ()
main = do

  let krotki = calc 654321 1 1 []
  print( krotki )
  let prods = [c | (_,_,c) <- krotki]
  print( prods )
  let noDups = removeDuplicates prods
  print( sum noDups )
