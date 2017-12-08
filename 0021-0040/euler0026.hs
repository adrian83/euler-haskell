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

data Cycle = Cycle {
  number :: Integer,
  len :: Int
} deriving (Show, Eq)


instance Ord Cycle where
  compare s1 s2
    | len s1 > len s2 = LT
    | len s1 < len s2 = GT
    | otherwise = EQ

genFract :: Integer -> Integer -> Integer -> Bool -> String
genFract licznik mianownik size start
  | size == 0               = ""
  | licznik == mianownik    = "1"
  | licznik > mianownik    = show (quot licznik mianownik) ++ if mod licznik mianownik == 0 then "" else genFract ((mod licznik mianownik)*10) mianownik (size-1) False
  | otherwise = (if start then "" else "0") ++ genFract (licznik*10) mianownik (size-1) False



recu :: String -> Int -> Maybe Int
recu [] _ = Nothing
recu tab len
  | len > half = Nothing
  | otherwise = if startsWith (fst spl) (snd spl) then Just len else recu tab (len+1)
    where
      half = fromIntegral $ quot (length tab) 2
      spl = splitAt len tab

fullRecu :: String -> Int
fullRecu [] = 0
fullRecu tab =
  let
    m = fullRecu (tail tab)
  in case recu tab 1 of
    Just n -> max n m
    Nothing -> m

startsWith :: String -> String -> Bool
startsWith [] _ = True
startsWith (a:xa) (b:xb) = a == b && startsWith xa xb


fractions :: Integer -> [(Integer, String)]
fractions maxx = [(i, genFract 1 i 800 True) | i <- [2,3..maxx]]

lenghts :: [(Integer, String)] -> [Cycle]
lenghts [] = []
lenghts (f:tps) = if null $ snd f
  then Cycle {number=(fst f), len=0} : lenghts tps
  else
    let r = fullRecu (snd f)
  in Cycle {number=(fst f), len=r}: lenghts tps


main :: IO ()
main = do

  print(head $ sort $ lenghts $ fractions 1000)
