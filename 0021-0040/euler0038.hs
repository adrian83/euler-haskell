-- https://projecteuler.net/problem=38
--
-- Take the number 192 and multiply it by each of 1, 2, and 3:
--   192 × 1 = 192
--   192 × 2 = 384
--   192 × 3 = 576
-- By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576
-- the concatenated product of 192 and (1,2,3)
-- The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the
-- pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).
-- What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product
-- of an integer with (1,2, ... , n) where n > 1?

import qualified Data.Map.Lazy as Map

digits :: Map.Map Char Bool
digits = Map.fromList [('1',True),('2',True),('3',True),('4',True),('5',True),('6',True),('7',True),('8',True),('9',True)]

containsZero :: String -> Bool
containsZero numberAsStr = elem '0' numberAsStr

hasDuplicatedDigits :: String -> Map.Map Char Bool -> Bool
hasDuplicatedDigits numberAsStr digsMap
  | null numberAsStr = False
  | null digsMap     = True
  | otherwise        = case maybeExists of
                        Just _ -> hasDuplicatedDigits (tail numberAsStr) (Map.delete (head numberAsStr) digsMap)
                        Nothing -> True
  where
    maybeExists = Map.lookup (head numberAsStr) digsMap


multiplications :: Integer -> Int -> Int -> [Integer] -> [Integer]
multiplications number _ _ [] = multiplications number 2 (length (show number)) [number]
multiplications number curMultiplier digits acc
  | newDigits > 9 = acc
  | otherwise     = multiplications number (curMultiplier+1) newDigits (acc ++ [newNumber])
  where
    newNumber = number * (toInteger curMultiplier)
    newDigits = digits + (length (show newNumber))


result :: Integer -> Integer -> Integer -> Integer
result number maxx best
  | number > maxx = best
  | otherwise     = result (number+1) maxx (if isPandigital then newNumber else best)
  where
    numbers = multiplications number 0 0 []
    numbsAsStr = foldl (\x y -> x ++ (show y)) "" numbers
    newNumber = (read numbsAsStr) :: Integer
    isPandigital = newNumber > best && ((not (containsZero numbsAsStr)) && (not (hasDuplicatedDigits numbsAsStr digits)))


main :: IO ()
main = print ("Expected: " ++ show (932718654 :: Integer) ++ ", actual: " ++ show (result 1 10000 0))
