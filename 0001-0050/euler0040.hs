-- https://projecteuler.net/problem=40
--
-- An irrational decimal fraction is created by concatenating the positive integers:
--    0.123456789101112131415161718192021...
-- It can be seen that the 12th digit of the fractional part is 1.
-- If dn represents the nth digit of the fractional part, find the value of the following expression.
--    d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000


import Data.Char

step :: Integer
step = 5

processFractionPart :: [Integer] -> Integer -> Integer -> String
processFractionPart [] _ _ = []
processFractionPart indexes curNumb curIndex = let
    nextNumb = curNumb + step
    numbsStr = foldl (\s1 s2 -> s1 ++ (show s2)) "" [curNumb..(nextNumb-1)]
    strLen = toInteger (length numbsStr)
    nextIndex = (head indexes) - 1
    inStr = nextIndex < curIndex + strLen
  in if inStr
    then (numbsStr !! ( fromIntegral (nextIndex - curIndex))) : (processFractionPart (tail indexes) nextNumb (curIndex + strLen))
    else processFractionPart indexes nextNumb (curIndex + strLen)

digitsProduct :: String -> Int
digitsProduct intAsStr = product $ map digitToInt intAsStr


main :: IO ()
main = do
  let indexes = [1,10,100,1000,10000,100000,1000000]
  print ("Expected: " ++ show (210 :: Integer) ++ ", actual: " ++ show (digitsProduct (processFractionPart indexes 1 0)))
