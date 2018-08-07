-- https://projecteuler.net/problem=33
--

data MyFraction = MyFraction {
    numerator :: Integer,
    denominator :: Integer
  } deriving (Show, Eq)

equal :: MyFraction -> MyFraction -> Bool
equal f1 f2 = ((numerator f1) * (denominator f2)) == ((denominator f1) * (numerator f2))

removeCommonDigit :: MyFraction -> MyFraction
removeCommonDigit fraction
  | head numeratorStr == last denominatorStr = MyFraction{ numerator = (read (tail numeratorStr) :: Integer), denominator = (read (init denominatorStr)) }
  | otherwise = MyFraction{ numerator = (read (init numeratorStr) :: Integer), denominator = (read (tail denominatorStr)) }
  where
    numeratorStr = show (numerator fraction)
    denominatorStr = show (denominator fraction)

nonTrivial :: MyFraction -> Bool
nonTrivial fraction = (head (show (numerator fraction))) == (last (show (denominator fraction))) || (head (show (denominator fraction))) == (last (show (numerator fraction)))

generator :: Integer -> Integer -> [MyFraction]
generator minn maxx = [MyFraction{numerator = j, denominator = i} | i <- [minn+1 .. maxx], j <- [minn .. i-1] ]

calculateFractions :: [MyFraction] -> [MyFraction]
calculateFractions [] = []
calculateFractions fractions =
  if nonTrivial (head fractions)
    then
      let
        simplified = removeCommonDigit (head fractions)
        equals = equal simplified (head fractions)
      in if equals then (head fractions) : (calculateFractions (tail fractions)) else calculateFractions (tail fractions)
    else calculateFractions (tail fractions)

myFractionsProduct :: [MyFraction] -> MyFraction
myFractionsProduct [] = error "cannot calculate product"
myFractionsProduct fractions = foldl (\f1 f2 -> MyFraction{numerator = (numerator f1 * numerator f2), denominator = (denominator f1 * denominator f2)}) MyFraction{numerator = 1, denominator = 1} fractions

greatestCommonDivisor :: Integer -> Integer -> Integer
greatestCommonDivisor a b
  | a == b = a
  | a > b = greatestCommonDivisor (a-b) b
  | otherwise = greatestCommonDivisor a (b-a)

result :: [MyFraction] -> Integer
result [] = 0
result fractions = quot (denominator fractionsProduct) myGcd
  where
    fractionsProduct = myFractionsProduct (calculateFractions fractions)
    myGcd = greatestCommonDivisor (numerator fractionsProduct) (denominator fractionsProduct)

main :: IO ()
main = print ("Expected: " ++ show (100 :: Integer) ++ ", actual: " ++ show (result (generator 10 100)))
