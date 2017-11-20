-- https://projecteuler.net/problem=29
--
-- Consider all integer combinations of ab for 2 ≤ a ≤ 5 and 2 ≤ b ≤ 5:
-- 22=4, 23=8, 24=16, 25=32
-- 32=9, 33=27, 34=81, 35=243
-- 42=16, 43=64, 44=256, 45=1024
-- 52=25, 53=125, 54=625, 55=3125
-- If they are then placed in numerical order, with any repeats removed, we get the following sequence of 15 distinct terms:
--    4, 8, 9, 16, 25, 27, 32, 64, 81, 125, 243, 256, 625, 1024, 3125
-- How many distinct terms are in the sequence generated by ab for 2 ≤ a ≤ 100 and 2 ≤ b ≤ 100?


import Data.List

powers :: Integer -> Integer -> [Integer]
powers number 2 = [number^(2::Integer)]
powers number maxExponent =  number^maxExponent : powers number (maxExponent-1)

allPowers :: Integer -> Integer -> [Integer]
allPowers 2 exnt = powers 2 exnt
allPowers maxNumber maxExponent = powers maxNumber maxExponent ++ allPowers (maxNumber-1) maxExponent

removeDuplicates :: [Integer] -> [Integer]
removeDuplicates [a] = [a]
removeDuplicates [a,b] = if a == b then [a] else [a,b]
removeDuplicates (a:b:xs) = if a == b then (removeDuplicates (b:xs)) else a:(removeDuplicates (b:xs))

distinctTerms :: Integer -> Integer -> Int
distinctTerms maxNumber maxExponent = length $ removeDuplicates $ sort $ allPowers maxNumber maxExponent

main :: IO ()
main = print ("Expected: " ++ show (9183 :: Integer) ++ ", actual: " ++ show (distinctTerms 100 100))
