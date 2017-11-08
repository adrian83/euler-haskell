-- https://projecteuler.net/problem=44

data Solution = Solution {
    p1 :: Integer,
    p2 :: Integer,
    pDiff :: Integer,
    pSum :: Integer
  } deriving (Show)

pentagonals :: Integer -> [Integer]
pentagonals maxx = [quot (n * ((3 * n) - 1)) 2 | n <- [1..maxx]]

solutions :: [Integer] -> [Solution]
solutions [] = []
solutions ps =
  let
    f = head ps
    r = tail ps
  in (map (\n -> Solution {p1=f, p2=n, pDiff=(n-f), pSum=(n+f)} ) r) ++ (solutions r)

sumAndDifArePentagonals :: [Integer] -> Solution -> Bool
sumAndDifArePentagonals pnts candidate = (pDiff candidate) `elem` pnts && (pSum candidate) `elem` pnts

main :: IO ()
main = do
  let pnts = pentagonals 18000
  print(filter (sumAndDifArePentagonals pnts) (solutions $ (take 3000 pnts)))
  --print (solutions $ pnts)
