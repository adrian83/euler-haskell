
-- https://projecteuler.net/problem=56
--
-- A googol (10^100) is a massive number: one followed by one-hundred zeros;
-- 100^100 is almost unimaginably large: one followed by two-hundred zeros.
-- Despite their size, the sum of the digits in each number is only 1.
-- Considering natural numbers of the form, a^b, where a, b < 100, what is the maximum digital sum?

digits :: (Integral a) => a -> [a]
digits 0 = []
digits n = mod n 10 : digits (quot n 10)

digitsSum :: Integer -> Integer
digitsSum a = sum $ digits a

numbers :: [Integer] -> [Integer] -> [Integer] -> [Integer]
numbers bases exponents acc
  | null exponents = acc
  | otherwise = numbers bases (tail exponents) (acc ++ map (^ head exponents) bases)

result :: Integer -> Integer -> Integer
result maxBase maxExponent = maximum $ map digitsSum (numbers [0,1..maxBase] [0,1..maxExponent] [])

main :: IO ()
main = print ("Result should be: " ++ show (972 :: Integer) ++ ", is: " ++ show (result 100 100))
