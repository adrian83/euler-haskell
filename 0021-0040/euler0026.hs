-- https://projecteuler.net/problem=26
--
-- A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:
--   1/2	= 	0.5
--   1/3	= 	0.(3)
--   1/4	= 	0.25
--   1/5	= 	0.2
--   1/6	= 	0.1(6)
--   1/7	= 	0.(142857)
--   1/8	= 	0.125
--   1/9	= 	0.(1)
--   1/10	= 	0.1
-- Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.
-- Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.

import Data.List
import Data.List.Split


data Cycle = Cycle {
  number :: Integer,
  len :: Int,
  str :: String
} deriving (Show, Eq)


fractionList :: Integer -> Integer -> Integer -> Bool -> String
fractionList numerator denominator resultSize start
  | resultSize < 1           = []
  | numerator == denominator = ['1']
  | numerator > denominator  = (show (quot numerator denominator)) ++ (if rest == 0 then [] else fractionList ((rest)*10) denominator (resultSize-1) False)
  | otherwise                = (if start then next else '0' : next)
  where
      rest = mod numerator denominator
      next = fractionList (numerator*10) denominator (resultSize-1) False


chunksEqual :: [String] -> Bool
chunksEqual [] = True
chunksEqual [_] = True
chunksEqual (a:ax) = if a == (head ax) then chunksEqual ax else False


splitToSize :: String -> Int -> [String]
splitToSize str size = filter (\e -> length e == size) (chunksOf size str)


longestCycle :: Integer -> String -> String -> Int -> Cycle
longestCycle _ _ [] _ = Cycle{number=0, len=0, str=""}
longestCycle denominator orgFractionStr fractionStr size =
  case splittedFull of
    [] -> longestCycle denominator orgFractionStr (tail fractionStr) 1
    [_] -> longestCycle denominator orgFractionStr (tail fractionStr) 1
    (a:ax) -> if chunksEqual splittedFull
                then Cycle{number=denominator, len=leng, str=orgFractionStr}
                else longestCycle denominator orgFractionStr fractionStr (size+1)
  where
    splittedFull = splitToSize fractionStr size
    leng = length (head splittedFull)


result :: Integer -> Integer -> Cycle
result 1 _ = Cycle{number=0, len=0, str=""}
result denominator maxFractionStrLength = if len this > len next then this else next
  where
    next = result (denominator - 1) maxFractionStrLength
    fractionStr = fractionList 1 denominator maxFractionStrLength True
    this = longestCycle denominator fractionStr fractionStr 1


main :: IO ()
main = print ("Expected: " ++ show (983 :: Integer) ++ ", actual: " ++ show (number (result 1000 5000)))
