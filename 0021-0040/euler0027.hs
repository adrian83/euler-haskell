-- https://projecteuler.net/problem=27
--
-- Euler discovered the remarkable quadratic formula:
--      n2+n+41
-- It turns out that the formula will produce 40 primes for the consecutive integer
-- values 0≤n≤39. However, when n=40,402+40+41=40(40+1)+41 is divisible by 41, and
-- certainly when n=41,412+41+41 is clearly divisible by 41.
-- The incredible formula n2−79n+1601 was discovered, which produces 80 primes for
-- the consecutive values 0≤n≤79. The product of the coefficients, −79 and 1601, is −126479.
-- Considering quadratics of the form:
--      n2+an+b, where |a|<1000 and |b|≤1000
--      where |n| is the modulus/absolute value of n
--      e.g. |11|=11 and |−4|=4
-- Find the product of the coefficients, a and b, for the quadratic expression that
-- produces the maximum number of primes for consecutive values of n, starting with n=0.



data Solution = Solution {
    len :: Integer,
    a :: Integer,
    b :: Integer
} deriving (Show, Eq)

instance Ord Solution where
    compare s1 s2
        | len s1 > len s2   = GT
        | len s1 == len s2  = EQ
        | otherwise         = LT

mulAB :: Solution -> Integer
mulAB sol = a sol * b sol


isPrime :: [Integer] -> Integer -> Bool
isPrime [] _ = False
isPrime primes number
  | number < 2            = False
  | head primes == number = True
  | head primes > number  = False
  | otherwise             = isPrime (tail primes) number

possiblePrime :: Integer -> Bool
possiblePrime n = n > 0 && ((elem n [0,1,2,3,5,7,11,13,17,19,23,29,31]) || (all (\h -> n `mod` h /= 0) [2,3,5,7,11,13,17,19,23,29,31]))


calculate :: Integer -> Integer -> Integer -> Integer
calculate a b n = (n^2) + (a * n) + b

longest :: (Integer -> Bool) -> [Integer] -> Integer -> Integer -> Integer
longest _ [] _ best = best
longest isPrime (n:numbers) current best = longest isPrime numbers newCurrent newBest
    where
        newCurrent = if isPrime n then current+1 else 0
        newBest = max newCurrent best

splitToPossiblePrimeSlices :: [Integer] -> [Integer] -> Integer -> Integer -> [(Integer, [Integer])]
splitToPossiblePrimeSlices [] acc accLen maxx = if accLen < maxx then [] else [(accLen, acc)]
splitToPossiblePrimeSlices (n:numbs) acc accLen maxx
    | prime = splitToPossiblePrimeSlices numbs (n:acc) (accLen+1) maxx
    | otherwise = if accLen < maxx then splitToPossiblePrimeSlices numbs [] 0 maxx else (accLen, acc) : (splitToPossiblePrimeSlices numbs [] 0 maxx)
    where
        prime = possiblePrime n


maxSolution :: [(Integer, [Integer])] -> (Integer -> Bool) -> Integer -> Integer -> Solution -> Solution
maxSolution [] _ _ _ best = best
maxSolution (sliceTuple:tuples) isPrime a b best = maxSolution tuples isPrime a b newBest
    where
        newBest = if (fst sliceTuple) < len best
            then best
            else max Solution {len = (longest isPrime (snd sliceTuple) 0 (len best)), a = a, b = b} best


calcForAB :: (Integer -> Bool) -> Integer -> Integer -> Solution -> Solution
calcForAB isPrime a b best = newLongest
    where
        ns = [calculate a b n | n <- [0,1..100]]
        possiblePrimeSlices = splitToPossiblePrimeSlices ns [] 0 (len best)
        newLongest = maxSolution possiblePrimeSlices isPrime a b best

calcForBs :: (Integer -> Bool) -> Integer -> [Integer] -> Solution -> Solution
calcForBs _ _ [] best = best
calcForBs isPrime a (b:bs) best = calcForBs isPrime a bs this
    where
        this = calcForAB isPrime a b best

calcForA :: (Integer -> Bool) -> Integer -> [Integer] -> Solution -> Solution
calcForA isPrime a bs best = sol
    where
        sol = calcForBs isPrime a bs best

calcForAs :: (Integer -> Bool) -> [Integer] -> [Integer] -> Solution -> Solution
calcForAs _ [] bs best = best
calcForAs isPrime (a:as) bs best = calcForAs isPrime as bs sol
    where
        sol = calcForA isPrime a bs best


main :: IO ()
main = do
  let maxN = 100
  let maxPrime = (maxN^2) + (1000*maxN) + 1000
  f <- readFile "../primes/primes"
  let primes = filter (\n -> n <= maxPrime) [read s :: Integer | s <- lines f]
  let isPrimeFunc = isPrime primes



  let asn = filter (\p -> (abs p) < 1000) (1:primes)
  let as = map (\n -> (-n)) asn ++ [0] ++ asn
  let bs = reverse $ filter (\p -> (abs p) < 1000) (0:1:primes)
  let r = calcForAs isPrimeFunc as bs Solution {len = 40, a = 1, b = 41}

  print ("Expected: " ++ show (-59231 :: Integer) ++ ", actual: " ++ show (mulAB r))
