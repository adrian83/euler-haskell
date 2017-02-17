-- https://projecteuler.net/problem=29


digits :: Integer -> [Integer]
digits 0 = []
digits i = (mod i 10) : digits (quot i 10)

isSumOfItsDigits :: Integer -> Bool
isSumOfItsDigits a = a == sum [i^5 | i <- (digits a)]


result :: Integer -> [Integer]
result maxNumber = [i | i <- [2,3..maxNumber], isSumOfItsDigits i]


main :: IO ()
main = do
  --print(pows 2 100)
  --print(length $ allNumbs 100)
  print(sum $ result 1000000 )
