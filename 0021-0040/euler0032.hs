-- https://projecteuler.net/problem=32
--
-- We shall say that an n-digit number is pandigital if it makes use of all the
-- digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.
-- The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.
-- Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
-- HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.

import Data.List

data Product = Product {
    a :: Integer,
    b :: Integer,
    axb :: Integer,
    str :: String
  } deriving (Show)

pandigital :: Product -> Bool
pandigital p = case length (str p) of
      0 -> False
      1 -> sorted == "1"
      2 -> sorted == "12"
      3 -> sorted == "123"
      4 -> sorted == "1234"
      5 -> sorted == "12345"
      6 -> sorted == "123456"
      7 -> sorted == "1234567"
      8 -> sorted == "12345678"
      9 -> sorted == "123456789"
      _ -> False
      where sorted = sort $ str p

products :: [([Integer], [Integer])] -> [Product]
products [] = []
products bnds = [Product {a=a, b=b, axb=a*b, str=((show a) ++ (show b) ++ (show (a * b)))} | a <- fst (head bnds), b <- snd (head bnds)] ++ (products $ tail bnds)

unique :: [Integer] -> [Integer] -> [Integer]
unique [] acc = acc
unique numbers acc = if (head numbers) `elem` acc then unique (tail numbers) acc else unique (tail numbers) ((head numbers):acc)

bounds :: [([Integer], [Integer])]
bounds = [([1..9],[1000..9999]), ([10..99],[100..999])]

result :: [([Integer], [Integer])] -> Integer
result bnds = sum (unique [ axb p | p <- (filter pandigital (products bnds))] [])

main :: IO ()
main = do
    print ("Expected: " ++ show (45228 :: Integer) ++ ", actual: " ++ show (result bounds))
