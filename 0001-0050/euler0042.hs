-- https://projecteuler.net/problem=42
--
-- The n^th term of the sequence of triangle numbers is given by, tn = ½n(n+1);
-- so the first ten triangle numbers are:
--    1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
-- By converting each letter in a word to a number corresponding to its alphabetical
-- position and adding these values we form a word value. For example, the
-- word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value is a
-- triangle number then we shall call the word a triangle word.
-- Using words.txt (right click and 'Save Link/Target As...'), a 16K text file
-- containing nearly two-thousand common English words, how many are triangle words?

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

wordsValues :: [String] -> [Integer]
wordsValues wordsList = [lettersSum word | word <- wordsList]

trianglesUpTo :: Integer -> [Integer]
trianglesUpTo maks = [ quot (i * (i+1)) 2 | i <- [1,2..maks]]

isTriangle :: [Integer] -> Integer -> Bool
isTriangle triangles e = e `elem` triangles

result :: [String] -> Int
result names =
  let
    values = wordsValues names
    triangles = trianglesUpTo $ maximum values
  in
    length $ filter (isTriangle triangles) values

main :: IO ()
main = do
  f <- readFile "./p042_words.txt"
  let namesWithQuotationMarks = splitBy (==',') f
  let names = map removeQuotationMarks namesWithQuotationMarks

  print ("Result should be: " ++ show (162 :: Integer) ++ ", is: " ++ show (result names))
