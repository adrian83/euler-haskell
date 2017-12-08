-- https://projecteuler.net/problem=21
--
-- Let d(n) be defined as the sum of proper divisors of n (numbers less
-- than n which divide evenly into n).
-- If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair
-- and each of a and b are called amicable numbers.
-- For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22,
-- 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284
-- are 1, 2, 4, 71 and 142; so d(284) = 220.
-- Evaluate the sum of all the amicable numbers under 10000.

dividors :: Integer -> [Integer]
dividors a = [ i | i <- [1,2..(quot a 2)], mod a i == 0]

isAmicable :: Integer -> Maybe (Integer, Integer)
isAmicable number =
  let
    divs = dividors number
    divsSum = sum divs
    divsDivs = dividors divsSum
    sumDivsDivs = sum divsDivs
    isAm = number /= divsSum && sumDivsDivs == number
  in if isAm then Just (number, divsSum) else Nothing

merge :: Integer -> Integer -> [Integer] -> [Integer]
merge a b amcbs =
  if a `elem` amcbs
    then if b `elem` amcbs then amcbs else b:amcbs
    else if b `elem` amcbs then a:amcbs else a:b:amcbs

amicables :: Integer -> [Integer]
amicables 0 = []
amicables number = case isAmicable number of
      Nothing -> amicables (number - 1)
      Just (f, s) -> merge f s (amicables (number - 1))

sumOfAmicables :: Integer -> Integer
sumOfAmicables maxNumber = sum $ amicables maxNumber

main :: IO ()
main = print ("Expected: " ++ show (31626 :: Integer) ++ ", actual: " ++ show (sumOfAmicables 10000))
