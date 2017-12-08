-- https://projecteuler.net/problem=22
--
-- Using names.txt (right click and 'Save Link/Target As...'), a 46K text file
-- containing over five-thousand first names, begin by sorting it into
-- alphabetical order. Then working out the alphabetical value for each name,
-- multiply this value by its alphabetical position in the list to obtain a name score.
--
-- For example, when the list is sorted into alphabetical order, COLIN,
-- which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list.
-- So, COLIN would obtain a score of 938 × 53 = 49714.
--
-- What is the total of all the name scores in the file?

import Data.List

letterValue :: Char -> Integer
letterValue 'A' = 1
letterValue 'B' = 2
letterValue 'C' = 3
letterValue 'D' = 4
letterValue 'E' = 5
letterValue 'F' = 6
letterValue 'G' = 7
letterValue 'H' = 8
letterValue 'I' = 9
letterValue 'J' = 10
letterValue 'K' = 11
letterValue 'L' = 12
letterValue 'M' = 13
letterValue 'N' = 14
letterValue 'O' = 15
letterValue 'P' = 16
letterValue 'Q' = 17
letterValue 'R' = 18
letterValue 'S' = 19
letterValue 'T' = 20
letterValue 'U' = 21
letterValue 'V' = 22
letterValue 'W' = 23
letterValue 'X' = 24
letterValue 'Y' = 25
letterValue 'Z' = 26
letterValue _ = error "Invalid letter"

splitBy :: (Char -> Bool) -> String -> [String]
splitBy p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitBy p s''
                            where (w, s'') = break p s'

lettersSum :: String -> Integer
lettersSum str = sum $ map letterValue str

removeQuotationMarks :: String -> String
removeQuotationMarks str = tail $ init str

namesProduct :: [String] -> Integer -> Integer
namesProduct [] _ = 0
namesProduct names index = (index * lettersSum (head names)) + (namesProduct (tail names) (index+1))

main :: IO ()
main = do
  f <- readFile "./p022_names.txt"
  let namesWithQuotationMarks = splitBy (==',') f
  let names = map removeQuotationMarks namesWithQuotationMarks

  print ("Expected: " ++ show (871198282 :: Integer) ++ ", actual: " ++ show (namesProduct (sort names) 1))
