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

result :: Integer -> Integer -> Integer
result 0 acc = acc
result a acc =
  let
    b = sum (dividors a)
  in
    if b < 10000 && a < 10000 && a /= b && a == (sum (dividors b)) then result (a-1) (acc+a+b) else result (a-1) acc

isAmicable :: Integer -> Bool
isAmicable number =
  let
    divs = dividors number
    divsSum = sum divs
    divsDivs = dividors divsSum
    sumDivsDivs = sum divsDivs
  in
    number /= divsSum && sumDivsDivs == number

result2 :: Integer -> [Integer]
result2 maxNumber = [i | i <- [2,3..maxNumber], isAmicable i]

main :: IO ()
main = do
  --print (dividors 220)
  --print (sum $ dividors 220)
  --print (dividors 284)
  --print (sum $ dividors 284)
  --print (result 15000 0)
  -- print(result 10000 0)
  print (isAmicable 220)
  print (sum $ result2 10000)
