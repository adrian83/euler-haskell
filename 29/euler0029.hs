import Data.List
-- https://projecteuler.net/problem=29


powers :: Integer -> Integer -> [Integer]
powers number maxExponent = if maxExponent == 2 then [number^2] else number^maxExponent : powers number (maxExponent-1)

allPowers :: Integer -> Integer -> [Integer]
allPowers maxNumber maxExponent = if maxNumber == 2 then powers maxNumber maxExponent else powers maxNumber maxExponent ++ (allPowers (maxNumber-1) maxExponent)

removeDuplicates :: [Integer] -> [Integer]
removeDuplicates [a] = [a]
removeDuplicates l =
  let
    f = head l
    t = tail l
  in
    if f == head t then removeDuplicates (f : tail t) else f : removeDuplicates t

main :: IO ()
main = do
  print( length $ removeDuplicates $ sort $ allPowers 100 100)
