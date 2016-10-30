
-- https://projecteuler.net/problem=4
--
-- A palindromic number reads the same both ways. The largest palindrome made
-- from the product of two 2-digit numbers is 9009 = 91 × 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.


isPalindrom :: (Eq a) => [a] -> Bool
isPalindrom [] = True
isPalindrom [a] = True
isPalindrom (x:xs) = if x == last xs then isPalindrom $ init xs else False

biggest :: (Integral a, Show a) => a -> a -> a -> a
biggest _ 999 c = c
biggest a b c =
    let m = a * b
    in if isPalindrom (show m) && m > c then biggest a (b+1) m else biggest a (b+1) c


biggest2 :: (Integral a, Show a) => a -> a -> a -> a
biggest2 999 _ c = c
biggest2 a b c =
    let nc = biggest a b c
    in if nc > c then biggest2 (a+1) b nc else biggest2 (a+1) b c


main = do
    print (biggest2 100 100 0)
