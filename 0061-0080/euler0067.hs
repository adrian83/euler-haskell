import Data.List.Split

merge :: [Integer] -> [Integer] -> [Integer]
merge [] [] = []
merge upper lower
  | length upper == 1 && length lower /= 1     = (head upper + head lower) : merge upper (tail lower)
  | length upper == 1 && length lower == 1     = [head upper + head lower]
  | length lower > length upper                = (head upper + head lower) : merge upper (tail lower)
  | otherwise                                  = ((max (upper !! 0) (upper !! 1)) + head lower) : merge (tail upper) (tail lower)

calculateLongestPaths :: [[Integer]] -> [Integer]
calculateLongestPaths [a] = a
calculateLongestPaths [a, b] = merge a b
calculateLongestPaths (x:y:xs) = calculateLongestPaths ((merge x y):xs)

longestPath :: [[Integer]] -> Integer
longestPath levels = maximum $ calculateLongestPaths levels

toIntegers :: String -> [Integer]
toIntegers text = map (\s -> read s :: Integer) (splitOn " " text)

main :: IO ()
main = do
    f <- readFile "./p067_triangle.txt"
    let triangle = [toIntegers s | s <- lines f]

    print ("Expected: " ++ show (7273 :: Integer) ++ ", actual: " ++ show (longestPath triangle))
