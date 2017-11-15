-- https://projecteuler.net/problem=17
--
-- If the numbers 1 to 5 are written out in words: one, two, three, four,
-- five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out
-- in words, how many letters would be used?

-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
-- forty-two) contains 23 letters and 115 (one hundred and fifteen)
-- contains 20 letters. The use of "and" when writing out numbers is
-- in compliance with British usage.


single :: Integer -> String
single 1 = "one"
single 2 = "two"
single 3 = "three"
single 4 = "four"
single 5 = "five"
single 6 = "six"
single 7 = "seven"
single 8 = "eight"
single 9 = "nine"
single _ = error "it should not happen"

double :: Integer -> String
double 10 = "ten"
double 11 = "eleven"
double 12 = "twelve"
double 13 = "thirteen"
double 14 = "fourteen"
double 15 = "fifteen"
double 16 = "sixteen"
double 17 = "seventeen"
double 18 = "eighteen"
double 19 = "nineteen"
double 20 = "twenty"
double 30 = "thirty"
double 40 = "forty"
double 50 = "fifty"
double 60 = "sixty"
double 70 = "seventy"
double 80 = "eighty"
double 90 = "ninety"
double n = double (quot n 10*10) ++ single (mod n 10)

hundred :: String
hundred = "hundred"

andd :: String
andd = "and"

triple :: Integer -> String
triple 100 = single 1 ++ hundred
triple 200 = single 2 ++ hundred
triple 300 = single 3 ++ hundred
triple 400 = single 4 ++ hundred
triple 500 = single 5 ++ hundred
triple 600 = single 6 ++ hundred
triple 700 = single 7 ++ hundred
triple 800 = single 8 ++ hundred
triple 900 = single 9 ++ hundred
triple 1000 = "onethousand"
triple n =
  let
    d = mod n 100
    t = quot n 100
    s = mod n 10
  in
    triple (t*100) ++ (if d > 9 then andd ++ double d else andd ++ single s)

numberToWord :: Integer -> String
numberToWord number
  | number < 1 || number > 1000 = error "number out of range"
  | number < 10                 = single number
  | number < 100                = double number
  | otherwise                   = triple number

numbersToString :: [Integer] -> String
numbersToString numbers = foldl (++) "" [ numberToWord n | n <- numbers ]

main :: IO ()
main = do
  --print ("Expected: " ++ show (19 :: Integer) ++ ", actual: " ++ show (length $ numbersToString [1,2..5]))
  print ("Expected: " ++ show (21124 :: Integer) ++ ", actual: " ++ show (length $ numbersToString [1,2..1000]))
