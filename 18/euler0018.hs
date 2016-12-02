-- https://projecteuler.net/problem=18
--

calc :: (Integral a) => [a] -> [a] -> [a]
calc _ [] = []
calc (x:xs) c = let
  n = head xs
  min' = if x < n then n else x
  in (min' + (head c)) : (calc xs (tail c))


ddd :: (Integral a) => [a] -> [a] -> [a]
ddd [] cur = cur
ddd [a] cur = [a+c | c <- cur]
ddd prev cur =
  let
    f = head cur
    l = last cur
    r = init $ tail cur
    nf = f + head prev
    nl = l + last prev
    nr = calc prev r
  in (nf : nr) ++ [nl]


cc :: (Integral a) => [a] -> [[a]] -> [a]
cc p [] = p
cc prev next =
  let
    n = head next
    newPrev = ddd prev n
  in
    cc newPrev (tail next)


main = do
  let lists = [[75],[95,64],[17,47,82],[18,35,87,10],[20,4,82,47,65],[19,01,23,75,03,34],[88,02,77,73,07,63,67],[99,65,04,28,06,16,70,92],[41,41,26,56,83,40,80,70,33],[41,48,72,33,47,32,37,16,94,29],[53,71,44,65,25,43,91,52,97,51,14],[70,11,33,28,77,73,17,78,39,68,17,57],[91,71,52,38,17,14,91,43,58,50,27,29,48],[63,66,04,68,89,53,67,30,73,16,69,87,40,31],[04,62,98,27,23,09,70,98,73,93,38,53,60,04,23]]
  print (maximum $ cc [] lists)
