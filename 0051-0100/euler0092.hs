-- https://projecteuler.net/problem=92
--
-- A number chain is created by continuously adding the square of the digits in a number to form a new number until it has been seen before.
-- For example,
-- 44 → 32 → 13 → 10 → 1 → 1
-- 85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89
-- Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop. What is most amazing is that EVERY starting number will eventually arrive at 1 or 89.
-- How many starting numbers below ten million will arrive at 89?

--import qualified Data.Map as Map
import qualified Data.Map.Lazy as Map

sumOfSqares :: [Integer] -> Integer
sumOfSqares [] = 0
sumOfSqares n = sum [i*i | i <- n]

digits :: Integer -> [Integer]
digits 0 = []
digits i = (mod i 10) : digits (quot i 10)

chainResult :: Map.Map Integer Integer -> Integer -> Integer
chainResult _ 1 = 1
chainResult _ 89 = 89
chainResult precomputed a = case Map.lookup a precomputed of
        Just n -> n
        Nothing -> chainResult precomputed (sumOfSqares $ digits a)

chainTo89 :: Map.Map Integer Integer -> Integer -> Bool
chainTo89 precomputed number = chainResult precomputed number == 89

arriveAt89 :: Integer -> Map.Map Integer Integer -> Int
arriveAt89 maxNumber precomputed = length $ filter (chainTo89 precomputed) [1,2..maxNumber]

genMap :: Integer -> Map.Map Integer Integer
genMap numb = Map.fromList (map (\n -> (n, chainResult Map.empty n)) [1,2..numb])

main :: IO ()
main = print ("Expected: " ++ show (8581146 :: Integer) ++ ", actual: " ++ show (arriveAt89 10000000 (genMap 600)))
