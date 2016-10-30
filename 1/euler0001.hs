
-- https://projecteuler.net/problem=1
-- 
-- If we list all the natural numbers below 10 that are multiples of 3 or 5,
-- we get 3, 5, 6 and 9. The sum of these multiples is 23.
-- Find the sum of all the multiples of 3 or 5 below 1000.


mysum :: (Integral n) => n -> n
mysum n = sum [x | x <- [1..(n-1)], (mod x 3) == 0 || (mod x 5) ==0]

main = do
    print (mysum 1000)
