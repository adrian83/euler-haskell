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

data Date = Date {
    year :: Integer,
    month :: Integer,
    day :: Integer
  } deriving (Show)

daysInMonth :: Integer -> Integer -> Integer
daysInMonth year month
  | month `elem` [4,6,9,11]        = 30
  | month `elem` [1,3,5,7,8,10,12] = 31
  | mod year 4 == 0                = 29
  | otherwise                      = 28

weekAfter :: Date -> Date
weekAfter date =
  if nextDay > daysInMth
    then if month date > 11
      then Date {year=(year date + 1), month=1, day=(nextDay - daysInMth) }
      else Date {year=year date, month=(month date + 1), day=(nextDay - daysInMth) }
    else Date {year=year date, month=month date, day=nextDay }
  where
    nextDay = (day date) + 7
    daysInMth = daysInMonth (year date) (month date)

isBefore :: Date -> Date -> Bool
isBefore first second = (year second > year first) || (year second == year first && month second > month first) || (year second == year first && month second == month first && day second > day first)

allSundaysBetween :: Date -> Date -> Date -> [Date]
allSundaysBetween sundayInPast start end
  | sundayInPast `isBefore` start = allSundaysBetween next start end
  | end `isBefore` sundayInPast = []
  | otherwise = next : allSundaysBetween next start end
  where
    next = weekAfter sundayInPast


firstInMonth :: Date -> Bool
firstInMonth date = day date == 1

firstSunday :: Date
firstSunday = Date {year=1900, month=1, day=7 }

startDate :: Date
startDate = Date {year=1901, month=1, day=1 }

endDate :: Date
endDate = Date {year=2000, month=12, day=31 }

numberOfSundaysAtTheFirstDayOfMonth :: Date -> Date -> Int
numberOfSundaysAtTheFirstDayOfMonth start end = length $ filter firstInMonth (allSundaysBetween firstSunday start end)

main :: IO ()
main = print ("Expected: " ++ show (171 :: Integer) ++ ", actual: " ++ show (numberOfSundaysAtTheFirstDayOfMonth startDate endDate))
