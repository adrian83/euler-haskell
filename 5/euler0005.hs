
-- https://projecteuler.net/problem=5
--
-- 2520 is the smallest number that can be divided by each of the numbers
-- from 1 to 10 without any remainder.
-- What is the smallest positive number that is evenly divisible by all of
-- the numbers from 1 to 20?


isDivisable :: (Integral a) => a -> [a] -> Bool
isDivisable _ [] = True
isDivisable n (x:xs) = if mod n x == 0 then isDivisable n xs else False

sm :: (Integral a) => [a] -> a
sm [] = 0
sm xs = head $ take 1 [i | i <- [20, 40..], isDivisable i xs]

main = do
    print (sm [20, 19 .. 1])
