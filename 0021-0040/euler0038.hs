-- https://projecteuler.net/problem=38
--

import qualified Data.Map.Lazy as Map

digits :: Map.Map Char Bool
digits = Map.fromList [('1',True),('2',True),('3',True),('4',True),('5',True),('6',True),('7',True),('8',True),('9',True)]

containsZero :: String -> Bool
containsZero numberAsStr = elem '0' numberAsStr

hasDuplicatedDigits :: String -> Map.Map Char Bool -> Bool
hasDuplicatedDigits numberAsStr digsMap
  | null numberAsStr = False
  | null digsMap = True
  | otherwise =
    let
      maybeExists = Map.lookup (head numberAsStr) digsMap
    in
      case maybeExists of
        Just _ -> hasDuplicatedDigits (tail numberAsStr) (Map.delete (head numberAsStr) digsMap)
        Nothing -> True

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
  | otherwise =
    let
      numbers = multiplications number 0 0 []
      numbsAsStr = foldl (\x y -> x ++ (show y)) "" numbers
      newNumber = (read numbsAsStr) :: Integer
    in
      if newNumber > best && ((not (containsZero numbsAsStr)) && (not (hasDuplicatedDigits numbsAsStr digits)))
        then result (number+1) maxx newNumber
        else result (number+1) maxx best



main :: IO ()
main = print ("Expected: " ++ show (932718654 :: Integer) ++ ", actual: " ++ show (result 1 10000 0))
