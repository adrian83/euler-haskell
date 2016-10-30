
-- https://projecteuler.net/problem=3
--
-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?

divisable :: (Integral a) => a -> [a] -> Bool
divisable 0 _ = False
divisable n [] = False
divisable n (x:xs) = if (mod n x) == 0 then True else divisable n xs


-- primes max current accumulator
primes :: (Integral a) => a -> a -> [a] -> [a]
primes 0 _ [_] = []
primes 1 _ [_] = []
primes m c acc = if c > m then acc else (if divisable c acc then primes m (c+1) acc else primes m (c+1) (acc++[c]))

biggestPF :: (Integral a) => a -> [a] -> a
biggestPF n l
    | (mod n la) == 0 = la
    | otherwise       = biggestPF n $ init l
    where la = last l


main = do
    let m =  600851475143
    let s = ceiling $ sqrt $ fromInteger m
    print (s)
    let p = primes s 6 [2, 3, 5]
    --print ( head $ primes 600851475143 3 [2])
    print (s)
    print ( biggestPF m p)
