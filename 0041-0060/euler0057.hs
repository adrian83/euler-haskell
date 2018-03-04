-- https://projecteuler.net/problem=57
-- It is possible to show that the square root of two can be expressed as an infinite continued fraction.
-- √ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
-- By expanding this for the first four iterations, we get:
-- 1 + 1/2 = 3/2 = 1.5
-- 1 + 1/(2 + 1/2) = 7/5 = 1.4
-- 1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
-- 1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...
-- The next three expansions are 99/70, 239/169, and 577/408, but the eighth expansion, 1393/985, is
-- the first example where the number of digits in the numerator exceeds the number of digits in the denominator.
-- In the first one-thousand expansions, how many fractions contain a numerator with more digits than denominator?


data Fraction = Fraction {
    numerator :: Integer,
    denumerator :: Integer
  } deriving (Show)

numeratorLonger :: Fraction -> Bool
numeratorLonger f = length (show (numerator f)) > length (show (denumerator f))

data Exp = SimpleExp Integer Integer Integer | ComplexExp Integer Integer Exp deriving (Show)

addInt :: Integer -> Exp -> Exp
addInt 0 ex = ex
addInt a ( SimpleExp b c d ) = SimpleExp (a+b) c d
addInt a ( ComplexExp b c d ) = ComplexExp (a+b) c d

generate :: Integer -> Exp
generate 0 = SimpleExp 1 1 2
generate a = ComplexExp 1 1 (addInt 1 (generate (a-1)))

toFraction :: Exp -> Fraction
toFraction (SimpleExp a b c) = Fraction {numerator=((a * c) + b), denumerator=c}
toFraction (ComplexExp a b c) =
  let
    t = toFraction c
    denum = numerator t
    num = (b * (denumerator t))
  in toFraction (SimpleExp a num denum)

longerNumeratorCount :: Integer -> Int
longerNumeratorCount maxx = length $ filter numeratorLonger (map (\n -> toFraction (generate n)) [0,1..maxx])

main :: IO ()
main = print ("Expected: " ++ show (153 :: Integer) ++ ", actual: " ++ show (longerNumeratorCount 1000))
