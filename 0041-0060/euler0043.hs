-- https://projecteuler.net/problem=43
-- The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the
-- digits 0 to 9 in some order, but it also has a rather interesting sub-string divisibility property.
-- Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:
-- d2d3d4=406 is divisible by 2
-- d3d4d5=063 is divisible by 3
-- d4d5d6=635 is divisible by 5
-- d5d6d7=357 is divisible by 7
-- d6d7d8=572 is divisible by 11
-- d7d8d9=728 is divisible by 13
-- d8d9d10=289 is divisible by 17
-- Find the sum of all 0 to 9 pandigital numbers with this property.

import Data.List

startsWithZero :: String -> Bool
startsWithZero txt = head txt == '0'

cond :: Int -> Int -> Integer -> String -> Bool
cond lowerIndex higherIndex divBy text = ((read (drop lowerIndex (take higherIndex text)) :: Integer) `mod` divBy) == 0

cond1 text = cond 1 4 2 text
cond2 text = cond 2 5 3 text
cond3 text = cond 3 6 5 text
cond4 text = cond 4 7 7 text
cond5 text = cond 5 8 11 text
cond6 text = cond 6 9 13 text
cond7 text = cond 7 10 17 text
conditions = [cond1, cond2, cond3, cond4, cond5, cond6, cond7]

allConds :: String -> [(String -> Bool)] -> Bool
allConds _ [] = True
allConds text conds = (head conds) text && allConds text (tail conds)

sumOfPalindromsFulfillingConditions :: String -> [(String -> Bool)] -> Integer
sumOfPalindromsFulfillingConditions base conds = sum $ map (\a -> read a :: Integer) (filter (\a -> allConds a conds) (filter (not . startsWithZero) (permutations base)))

main :: IO ()
main = print ("Expected: " ++ show (16695334890 :: Integer) ++ ", actual: " ++ show (sumOfPalindromsFulfillingConditions "0123456789" conditions))
