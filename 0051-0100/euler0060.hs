import Data.List

concToPrime :: [Integer] -> Integer -> Integer -> Bool
concToPrime primes prime1 prime2 = elem (read (show prime1 ++ show prime2)) primes && elem (read (show prime2 ++ show prime1)) primes

concToPrime2 :: [Integer] -> [Integer] -> Bool
concToPrime2 _ [] = True
concToPrime2 [] _ = False
concToPrime2 primes tested = if head tested > head primes
  then concToPrime2 (tail primes) tested
  else
    if head tested < head primes
      then False
      else concToPrime2 primes (tail tested)


allConcToPrime :: [Integer] -> [Integer] -> Bool
allConcToPrime _ [] = True
allConcToPrime _ [_] = True
allConcToPrime primes tested =
    let
        f = head tested
        bools = [concToPrime primes f a | a <- tail tested]
        allb = all (\a -> a) bools
    in
        allb && allConcToPrime primes (tail tested)

allConcToPrime2 :: [Integer] -> [Integer] -> [Integer] -> Bool
allConcToPrime2 primes tested acc =
  if null tested
    then concToPrime2 primes (sort acc)
    else
      let
        f = head tested
        n = [ read (show f ++ show a) | a <- tail tested] ++ [ read (show a ++ show f) | a <- tail tested]
      in
        allConcToPrime2 primes (tail tested) (acc ++ n)


result :: [Integer] -> [Integer] -> Integer -> [Integer] -> [Integer]
result [] _ _ _ = []
result primes curPrimes count acc =
    if toInteger (length acc) == count
        then acc
        else
            if null curPrimes
                then result (tail primes) (tail primes) count []
                else
                    let
                        testAcc = (head curPrimes) : acc
                        concToPr = allConcToPrime2 primes (sort testAcc) []
                    in
                        if concToPr
                            then result primes (tail curPrimes) count testAcc
                            else result primes (tail curPrimes) count acc

main :: IO ()
main = do
  f <- readFile "../primes/primes"
  let primes = [read s :: Integer | s <- lines f]

  --print (allConcToPrime primes [3,7])
  --print (allConcToPrime primes [3])
  --print (allConcToPrime primes [])

  print (result primes primes 3 [])

  --print( allConcToPrime2 primes [3,7,109,673] [] )
