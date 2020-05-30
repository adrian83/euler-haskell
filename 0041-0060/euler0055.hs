-- https://projecteuler.net/problem=55
--
-- If we take 47, reverse and add, 47 + 74 = 121, which is palindromic.
-- Not all numbers produce palindromes so quickly. For example,
--     349 + 943 = 1292,
--     1292 + 2921 = 4213
--     4213 + 3124 = 7337
-- That is, 349 took three iterations to arrive at a palindrome.
-- Although no one has proved it yet, it is thought that some numbers, like 196, never produce a palindrome. A number that never forms a palindrome through the reverse and add process is called a Lychrel number. Due to the theoretical nature of these numbers, and for the purpose of this problem, we shall assume that a number is Lychrel until proven otherwise. In addition you are given that for every number below ten-thousand, it will either (i) become a palindrome in less than fifty iterations, or, (ii) no one, with all the computing power that exists, has managed so far to map it to a palindrome. In fact, 10677 is the first number to be shown to require over fifty iterations before producing a palindrome: 4668731596684224866951378664 (53 iterations, 28-digits).
-- Surprisingly, there are palindromic numbers that are themselves Lychrel numbers; the first example is 4994.
-- How many Lychrel numbers are there below ten-thousand?
-- NOTE: Wording was modified slightly on 24 April 2007 to emphasise the theoretical nature of Lychrel numbers.
-- Not all numbers produce palindromes so quickly. For example,
--     349 + 943 = 1292,
--     1292 + 2921 = 4213
--     4213 + 3124 = 7337
-- That is, 349 took three iterations to arrive at a palindrome.
-- Although no one has proved it yet, it is thought that some numbers, like 196, never produce a palindrome. A number that never forms a palindrome through the reverse and add process is called a Lychrel number. Due to the theoretical nature of these numbers, and for the purpose of this problem, we shall assume that a number is Lychrel until proven otherwise. In addition you are given that for every number below ten-thousand, it will either (i) become a palindrome in less than fifty iterations, or, (ii) no one, with all the computing power that exists, has managed so far to map it to a palindrome. In fact, 10677 is the first number to be shown to require over fifty iterations before producing a palindrome: 4668731596684224866951378664 (53 iterations, 28-digits).
-- Surprisingly, there are palindromic numbers that are themselves Lychrel numbers; the first example is 4994.
-- How many Lychrel numbers are there below ten-thousand?
-- NOTE: Wording was modified slightly on 24 April 2007 to emphasise the theoretical nature of Lychrel numbers.

reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

removeLeadingZeros :: String -> String
removeLeadingZeros s = if head s == '0' then removeLeadingZeros (tail s) else s

reverseNumber :: Integer -> Integer
reverseNumber n = 
    let str = show n
        strRev = removeLeadingZeros (reverseList str)
    in read strRev :: Integer

isPalindrom :: (Eq a) => [a] -> Bool
isPalindrom [] = True
isPalindrom [_] = True
isPalindrom (x:xs) = (x == last xs) && isPalindrom (init xs)

isNumberPalindrom :: Integer -> Bool
isNumberPalindrom n = isPalindrom (show n)

isLychrelAfterIterations :: Integer -> Integer -> Bool
isLychrelAfterIterations n 0 = if isNumberPalindrom n then False else True
isLychrelAfterIterations n i = 
    let r = reverseNumber n
    in if isNumberPalindrom (n + r) then False else isLychrelAfterIterations (n + r) (i - 1)

lychrelBetween :: Integer -> Integer -> [Integer]
lychrelBetween minn maxx = filter (\n -> isLychrelAfterIterations n 50) [i | i <- [minn..maxx]]

main :: IO ()
main = do
    let result = lychrelBetween 1 10000
    print ("Expected: " ++ show (249 :: Integer) ++ ", actual: " ++ show (length result))
