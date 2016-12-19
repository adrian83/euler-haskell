-- https://projecteuler.net/problem=125
--
-- The palindromic number 595 is interesting because it can be written as the sum of
-- consecutive squares: 6^2 + 7^2 + 8^2 + 9^2 + 10^2 + 11^2 + 12^2.
-- There are exactly eleven palindromes below one-thousand that can be written as consecutive
-- square sums, and the sum of these palindromes is 4164. Note that 1 = 0^2 + 1^2 has not
-- been included as this problem is concerned with the squares of positive integers.
-- Find the sum of all the numbers less than 10^8 that are both palindromic and can be
-- written as the sum of consecutive squares.

-- digits returns digits of given number.
digits :: Integer -> [Integer]
digits 0 = []
digits i = (mod i 10) : digits (quot i 10)

-- palindrome returns True if given list is palindrome.
palindrome :: [Integer] -> Bool
palindrome [] = True
palindrome [_] = True
palindrome (x:xs) = if x == (last xs) then palindrome $ init xs else False

-- sums returns list of squeres of numbers less or equal 1st arg.
sums :: Integer -> [Integer]
sums 0 = []
sums n = (n*n) : sums (n-1)

--
isBla :: (Integral a) => a -> a -> [a] -> [a] -> Bool
isBla numb acc [] org = if numb == acc then True else False
isBla numb acc numbs org
  | numb > acc    = isBla numb (acc + (head numbs)) (tail numbs) org
  | numb == acc   = True
  | otherwise     = isBla numb (acc - (head org)) numbs (tail org)


isCostam :: (Integral a) => a -> a
isCostam numb =
  let
    pierw = floor $ sqrt $ fromIntegral numb
  in
    if numb == (pierw*pierw) then 0 else (
      let
        sqrs = if pierw < 3 then [4,1] else [ a*a | a <- [pierw,(pierw-1)..1]]
      in
        if isBla numb 0 sqrs sqrs then numb else 0
      )
    )

res :: (Integral a) => a -> [a]
res maks = let
    palins = [n | n <- [5..maks], palindrome $ digits n]
  in
    [isCostam p | p <- palins ]

main = do
  print ([5,4..1])
  print( res (10^3))
  print( sum $ res (10^8))
