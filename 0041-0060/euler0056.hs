-- https://projecteuler.net/problem=56
--
-- A googol (10^100) is a massive number: one followed by one-hundred zeros;
-- 100^100 is almost unimaginably large: one followed by two-hundred zeros.
-- Despite their size, the sum of the digits in each number is only 1.
-- Considering natural numbers of the form, a^b, where a, b < 100, what is the maximum digital sum?

digits :: (Integral a) => a -> [a]
digits 0 = []
digits n = mod n 10 : digits (quot n 10)

genNumbers :: Integer -> Integer -> [Integer]
genNumbers maxBase maxExponent = [ b ^ e | b <- [1,2..maxBase], e <- [1,2..maxExponent]]

biggestDigitsSum :: [Integer] -> Integer
biggestDigitsSum numbers = maximum (map sum (map digits numbers))

main :: IO ()
main = print ("Expected: " ++ show (972 :: Integer) ++ ", actual: " ++ show (biggestDigitsSum (genNumbers 100 100)))
