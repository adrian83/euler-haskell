-- https://projecteuler.net/problem=125
--
-- The palindromic number 595 is interesting because it can be written as the sum of
-- consecutive squares: 6^2 + 7^2 + 8^2 + 9^2 + 10^2 + 11^2 + 12^2.
-- There are exactly eleven palindromes below one-thousand that can be written as consecutive
-- square sums, and the sum of these palindromes is 4164. Note that 1 = 0^2 + 1^2 has not
-- been included as this problem is concerned with the squares of positive integers.
-- Find the sum of all the numbers less than 10^8 that are both palindromic and can be
-- written as the sum of consecutive squares.

import Data.List
import qualified Data.Map.Lazy as Map

isPalindrom :: (Eq a) => [a] -> Bool
isPalindrom [] = True
isPalindrom [_] = True
isPalindrom (x:xs) = (x == last xs) && isPalindrom (init xs)

sumSquares :: Integer -> Integer -> Integer -> [Integer]
sumSquares maxx numb summ = if n > maxx then [] else n : (sumSquares maxx (numb+1) n)
    where n = (numb^2)+summ

squaresSum :: Integer -> Integer -> [Integer]
squaresSum number maxx
    | fst t       = []
    | otherwise   = (snd t) ++ squaresSum (number+1) maxx
    where
        x = sumSquares maxx number 0
        t = case x of
            [] -> (True, [])
            [_] -> (True, [])
            x:xs -> (False, xs)

unique :: [Integer] -> [Integer] -> [Integer]
unique [] acc = acc
unique (x:xs) acc = if elem x acc then unique xs acc else unique xs (x:acc)

sumOfConsecutiveSquares :: Integer -> Integer
sumOfConsecutiveSquares maxx = sum $ unique [ i | i <- (squaresSum 1 maxx), isPalindrom (show i)] []

main :: IO ()
main = print ("Expected: " ++ show (2906969179 :: Integer) ++ ", actual: " ++ show (sumOfConsecutiveSquares (10^8)))
