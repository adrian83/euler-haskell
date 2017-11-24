-- https://projecteuler.net/problem=125
--
-- The palindromic number 595 is interesting because it can be written as the sum of
-- consecutive squares: 6^2 + 7^2 + 8^2 + 9^2 + 10^2 + 11^2 + 12^2.
-- There are exactly eleven palindromes below one-thousand that can be written as consecutive
-- square sums, and the sum of these palindromes is 4164. Note that 1 = 0^2 + 1^2 has not
-- been included as this problem is concerned with the squares of positive integers.
-- Find the sum of all the numbers less than 10^8 that are both palindromic and can be
-- written as the sum of consecutive squares.

import Data.List
import qualified Data.Map.Lazy as Map

data Entry = Entry {
    number :: Integer,
    prevs :: [Integer],
    sums :: [Integer]
  } deriving (Show)

genEntries :: Integer -> [Entry]
genEntries 2 = [Entry {number=2, prevs=[4,1], sums=[5,1]}]
genEntries number =
  let
    prevList = genEntries (number - 1)
    prev = head prevList
    sq = number ^ 2
    entry = Entry {number=number, prevs=(sq : prevs prev), sums=((makeSums sq (prevs prev)) ++ sums prev) }
  in entry : prevList

makeSums :: Integer -> [Integer] -> [Integer]
makeSums _ [] = []
makeSums number numbers
  | newNumber > (10^8) = makeSums newNumber (tail numbers)
  | otherwise = newNumber : makeSums newNumber (tail numbers)
  where newNumber = (number + head numbers)

isPalindrom :: (Eq a) => [a] -> Bool
isPalindrom [] = True
isPalindrom [_] = True
isPalindrom (x:xs) = (x == last xs) && isPalindrom (init xs)

digits :: Integer -> [Integer]
digits 0 = []
digits n = mod n 10 : digits (quot n 10)

palindroms :: Integer -> [Integer]
palindroms maxx = [p | p <- [1,2..maxx], isPalindrom $ digits p]

isSum :: Map.Map Integer Entry -> Integer -> Bool
isSum dict number = case Map.lookup n dict of
  Nothing -> False
  Just e -> number `elem` (sums e)
  where n = floor (sqrt  $ fromIntegral number)

palindromsAsSums :: Integer -> (Integer -> Bool) -> [Integer]
palindromsAsSums maxx isSumOfSqrs = [i | i <- (map (\e -> read e :: Integer) $ filter (\s -> (head s) /= '0') (genPalindroms "1234567890" 8)), isSumOfSqrs i ]

conc :: [String] -> String -> [String]
conc [] _ = []
conc l [] = map (\s -> s ++ (reverse s)) l
conc l [a] = map (\s -> s ++ (a : (reverse s))) l
conc l (x:xs) = (map (\s -> s ++ (x : (reverse s))) l) ++ (conc l xs)

genPalindroms :: String -> Integer -> [String]
genPalindroms elems 1 = [x | x <- mapM (const elems) [1] ]
genPalindroms elems len
  | odd len = (conc [x | x <- mapM (const elems) [1,2..newLen] ] elems) ++ genPalindroms elems (len-1)
  | otherwise = (conc [x | x <- mapM (const elems) [1,2..newLen] ] []) ++ genPalindroms elems (len-1)
    where newLen = quot len 2

main :: IO ()
main = do

  --print( conc ["ab", "cd"] "123")

  --print( take 20 $ sort $ (map (\e -> read e :: Integer) $ filter (\s -> (head s) /= '0') (genPalindroms "1234567890" 9)))

  let entries = genEntries (10^4)
  let entriesMap = Map.fromList (map (\e -> (number e, e)) entries)
  let fltr = isSum entriesMap
  print(sum $ palindromsAsSums (10^7) fltr)
