-- https://projecteuler.net/problem=63
--
-- The 5-digit number, 16807=75, is also a fifth power. Similarly, the 9-digit
-- number, 134217728=89, is a ninth power.
-- How many n-digit positive integers exist which are also an nth power?

isNthPower :: Integer -> Integer -> Integer -> Integer -> [Integer]
isNthPower curNumber ex minn maxx =
    if curNumber ^ ex > maxx
        then []
        else
            if curNumber ^ ex >= minn
                then (curNumber ^ ex) : isNthPower (curNumber+1) ex minn maxx
                else isNthPower (curNumber+1) ex minn maxx

solutionsWithDigits :: Integer -> [Integer]
solutionsWithDigits digits =
    let
        minn = 10 ^ (digits-1)
        maxx = (10 ^ digits) -1
    in isNthPower 1 digits minn maxx

solutions :: Integer -> [Integer]
solutions 0 = []
solutions maxEx = solutionsWithDigits maxEx ++ solutions (maxEx - 1)

main :: IO ()
main = print ("Expected: " ++ show (49 :: Integer) ++ ", actual: " ++ show (length $ solutions 49))
