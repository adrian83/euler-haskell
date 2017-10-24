-- https://projecteuler.net/problem=32
--
-- We shall say that an n-digit number is pandigital if it makes use of all the
-- digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.
-- The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.
-- Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
-- HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.

import Data.List


bounds :: [([Integer], [Integer])]
bounds = [([1..9],[1000..9999]), ([10..99],[100..999])]

products :: [([Integer], [Integer])] -> [Product]
products [] = []
products bnds = [Product {a=a, b=b, axb=a*b, str=((show a) ++ (show b) ++ (show (a * b)))} | a <- fst (head bnds), b <- snd (head bnds)] ++ (products $ tail bnds)

gg :: Int -> String -> Bool
gg 0 _ = False
gg 1 str = str == "1"
gg 2 str = str == "12"
gg 3 str = str == "123"
gg 4 str = str == "1234"
gg 5 str = str == "12345"
gg 6 str = str == "123456"
gg 7 str = str == "1234567"
gg 8 str = str == "12345678"
gg 9 str = str == "123456789"
gg _ _ = False


len :: Product -> Int
len p = length $ str p



pandigital :: Product -> Bool
pandigital p = let
  sortedStr = sort $ str p
  in gg (length sortedStr) sortedStr

data Product = Product {
    a :: Integer,
    b :: Integer,
    axb :: Integer,
    str :: String
  } deriving (Show)

unique :: [Integer] -> [Integer] -> [Integer]
unique [] acc = acc
unique numbers acc = if (head numbers) `elem` acc then unique (tail numbers) acc else unique (tail numbers) ((head numbers):acc)


main :: IO ()
main = do
    print (sum (unique [ axb p | p <- (filter (\p -> len p == 9 && pandigital p) (products bounds))] []))

  --print (take 3  (filter (\p -> len p == 9 && pandigital p) (products bounds)))
  --print (sum [ axb p | p <- (filter (\p -> len p == 9 && pandigital p) (products bounds))])
