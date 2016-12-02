
-- https://projecteuler.net/problem=3
--
-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?

-- returns True if given number is divisable by any of the number from given list, False otherwise
divisable :: (Integral a) => a -> [a] -> Bool
divisable 0 _ = False
divisable n [] = False
divisable n (x:xs) = if mod n x == 0 then True else divisable n xs

-- calculates all prime numbers smaller than first arg, currently checked
-- number is the second arg and all primes smaller then the first arg should be in third arg
-- primes max current accumulator
primes :: (Integral a) => a -> a -> [a] -> [a]
primes 0 _ [_] = []
primes 1 _ [_] = []
primes m c acc
    | c > m     = acc
    | otherwise = if divisable c acc then primes m (c+1) acc else primes m (c+1) (acc++[c])

-- returns biggest prime factor which is the biggest prime from the
-- second arg that divides given number in first arg
biggestPF :: (Integral a) => a -> [a] -> a
biggestPF n l
    | (mod n la) == 0 = la
    | otherwise       = biggestPF n $ init l
    where la = last l


main = do
    let max' = 600851475143
    let maxPrime = ceiling $ sqrt $ fromInteger max'
    let primeNumbers = primes maxPrime 6 [2, 3, 5]
    let biggest = biggestPF max' primeNumbers
    print(biggest)
