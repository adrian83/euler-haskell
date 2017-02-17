-- https://projecteuler.net/problem=19
--
-- You are given the following information, but you may prefer to do some
-- research for yourself.
-- 1. 1 Jan 1900 was a Monday.
-- 2. Thirty days has September,
--    April, June and November.
--    All the rest have thirty-one,
--    Saving February alone,
--    Which has twenty-eight, rain or shine.
--    And on leap years, twenty-nine.
-- 3. A leap year occurs on any year evenly divisible by 4, but not on a
-- century unless it is divisible by 400.
-- How many Sundays fell on the first of the month during the twentieth
-- century (1 Jan 1901 to 31 Dec 2000)?

year :: (Integer, Integer, Integer) -> Integer
year (_,_,y) = y

month :: (Integer, Integer, Integer) -> Integer
month (_,m,_) = m

day :: (Integer, Integer, Integer) -> Integer
day (d,_,_) = d


firstSunday :: (Integer, Integer, Integer)
firstSunday = (7, 1, 1900)

before :: (Integer, Integer, Integer) -> (Integer, Integer, Integer) -> Bool
before first sec =
  if year first > year second
    then False
    else
      if year first == year second
        then

sundaysUntil :: (Integer, Integer, Integer) -> (Integer, Integer, Integer) -> [(Integer, Integer, Integer)] -> [(Integer, Integer, Integer)]
sundaysUntil from to =




daysInYear :: Integer -> Integer
daysInYear y
  | mod y 4 == 0 = 365
  | otherwise = 366

days :: Integer -> Integer
days n = foldl (+) 0 [daysInYear i | i <- [1901,1902..2000]]


main :: IO ()
main = do
  print(quot ((days 7) - 6) 7)
