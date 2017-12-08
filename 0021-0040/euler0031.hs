-- https://projecteuler.net/problem=31

-- In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:
--     1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
-- It is possible to make £2 in the following way:
--     1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
-- How many different ways can £2 be made using any number of coins?

coins :: [Integer]
coins = [200, 100, 50, 20, 10, 5, 2, 1]

result :: Integer -> [Integer] -> Integer -> Integer
result _ [] acc = acc
result left coinss acc
  | head coinss > left = result left (tail coinss) acc
  | head coinss < left = result (left - head coinss) coinss acc + result left (tail coinss) acc
  | otherwise          = (acc + 1) + result left (tail coinss) acc

main :: IO ()
main = print ("Expected: " ++ show (73682 :: Integer) ++ ", actual: " ++ show (result 200 coins 0))
