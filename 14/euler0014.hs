-- https://projecteuler.net/problem=14
--
--The following iterative sequence is defined for the set of positive integers:
--n → n/2 (n is even)
--n → 3n + 1 (n is odd)
--Using the rule above and starting with 13, we generate the following sequence:
--13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
--It can be seen that this sequence (starting at 13 and finishing at 1)
--contains 10 terms. Although it has not been proved yet (Collatz Problem), it
--is thought that all starting numbers finish at 1.
--Which starting number, under one million, produces the longest chain?
--NOTE: Once the chain starts the terms are allowed to go above one million.

coll :: (Integral a) => a -> a -> a
coll n r = if n == 1 then r else (
    if mod n 2 == 0 then coll (quot n 2) (r+1) else coll ((3*n)+1) (r+1)
  )

longest :: (Integral a, Ord a) => a -> a -> (a,a) -> a
longest maks cur best = if cur == maks then fst best else (
    let
      collz = coll cur 1
    in
      if collz > snd best then longest maks (cur+1) (cur,collz) else longest maks (cur+1) best
  )

main = do
  print (longest 1000000 1 (0,0))
