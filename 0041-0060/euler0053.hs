-- https://projecteuler.net/problem=53
--



import qualified Data.Map.Lazy as Map


factorials :: Integer -> Map.Map Integer Integer -> Map.Map Integer Integer
factorials 1 acc = Map.insert 1 1 acc
factorials n acc = case Map.lookup (n-1) acc of
        Just nmo -> Map.insert n (n * nmo) acc
        Nothing -> let 
          newAcc = factorials (n-1) acc
          in case Map.lookup (n-1) newAcc of
            Just nmo2 -> Map.insert n (n * nmo2) newAcc
            Nothing -> error "Invalid"


selections :: Integer -> Integer -> Map.Map Integer Integer -> Integer
selections n r fs =
  let 
    nFa = case Map.lookup n fs of 
      Just facn -> facn
      Nothing -> error "no n in factorials"
    rFa = case Map.lookup r fs of 
      Just facr -> facr
      Nothing -> error "no r in factorials"
    nmrFa = case Map.lookup (n-r) fs of 
      Just facnmr -> facnmr
      Nothing -> error "no (n - r) in factorials"
  in  
    div nFa (rFa * nmrFa)


allSelections :: Integer -> Map.Map Integer Integer -> [Integer]
allSelections 3 _ = []
allSelections n acc = [ selections n r acc | r <- [1,2..(n-1)]] ++ allSelections (n-1) acc


main :: IO ()
main = do 
  let fs = (factorials 101 Map.empty)
  let allSelsGreaterThanOneMillion = filter (\n -> n > 1000000) (allSelections 100 fs)
  print ("Expected: " ++ show (4075 :: Integer) ++ ", actual: " ++ show (length allSelsGreaterThanOneMillion))
