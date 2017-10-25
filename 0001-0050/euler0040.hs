-- https://projecteuler.net/problem=40

--
import Data.Char

result :: [Integer] -> Integer -> Integer -> [Char] -> String
result [] _ _ acc = acc
result indexes curNumb curIndex acc = let
    nextNumb = curNumb + 5
    numbsStr = foldl (\s1 s2 -> s1 ++ (show s2)) "" [curNumb..(nextNumb-1)]
    strLen = toInteger (length numbsStr)
    nextIndex = (head indexes) - 1
    inStr = nextIndex < curIndex + strLen
  in if inStr
    then result (tail indexes) nextNumb (curIndex + strLen) ( (numbsStr !! ( fromIntegral (nextIndex - curIndex))) : acc)
    --(acc ++ "_" ++ numbsStr ++ "_" ++ show (numbsStr !! ( fromIntegral (nextIndex - curIndex))))
    else result indexes nextNumb (curIndex + strLen) acc


main :: IO ()
main = do
  print( foldl (\s1 s2 -> s1 ++ (show s2)) "" [1..20])
  print( product $ map digitToInt (result [1,10,100,1000,10000,100000,1000000] 1 0 []))
