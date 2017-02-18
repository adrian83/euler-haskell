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

daysInMonth :: Integer -> Integer -> Integer
daysInMonth year month
  | month `elem` [4,6,9,11]        = 30
  | month `elem` [1,3,5,7,8,10,12] = 31
  | mod year 4 == 0                = 29
  | otherwise                      = 28
  

nextSunday :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
nextSunday (d,m,y) =
  let
    nd = d + 7
    daysInM = daysInMonth y m
  in
    if nd > daysInM
      then (if m > 11 then (nd-daysInM, 1, y+1) else (nd-daysInM, m+1, y))
      else (nd, m, y)



before :: (Integer, Integer, Integer) -> (Integer, Integer, Integer) -> Bool
before (fd,fm,fy) (sd,sm,sy) = (sy > fy) || (sy == fy && sm > fm) || (sy == fy && sm == fm && sd > fd)


allSundaysBetween :: (Integer, Integer, Integer) -> (Integer, Integer, Integer) -> [(Integer, Integer, Integer)]
allSundaysBetween start end =
  if before start end
    then
      let
        next = nextSunday start
      in
        start : allSundaysBetween next end
    else []


firstInMonth :: (Integer, Integer, Integer) -> Bool
firstInMonth (d,_,_) = d == 1


firstSunday :: (Integer, Integer, Integer)
firstSunday = (7, 1, 1900)

startDate :: (Integer, Integer, Integer)
startDate = (1, 1, 1901)

endDate :: (Integer, Integer, Integer)
endDate = (31, 12, 2000)

main :: IO ()
main = do


  let allSundays = allSundaysBetween (5,1,1901) (31,12,2000)
  --let allSundays = allSundaysBetween (31,12,1900) (9,3,1901)
  --print (allSundays )
  print (length $ filter firstInMonth allSundays )
  print (last allSundays )
