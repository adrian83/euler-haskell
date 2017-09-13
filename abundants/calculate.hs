import Data.List

dividers :: Integer -> [Integer]
dividers a = [ i | i <- [1,2..(quot a 2)], mod a i == 0]

abundant :: Integer -> Bool
abundant number = if even number || mod number 5 == 0
  then number < sum (dividers number)
  else False

abundants :: Integer -> [Integer]
abundants maxx = [i | i <- [1,2..maxx], abundant i]

toListOfIntegers :: [String] -> [Integer]
toListOfIntegers strings = [read s :: Integer | s <- strings]

main :: IO ()
main = do
  f   <- readFile "./abundants"
  let smallerAbundants = toListOfIntegers (lines f)
  print ( show (length smallerAbundants) ++ " abundants already in file")
  let maxNumber = 30000
  let numbers = abundants maxNumber
  let abundantsstr = intercalate "\n" [show i | i <- numbers]
  writeFile "./abundants" abundantsstr
