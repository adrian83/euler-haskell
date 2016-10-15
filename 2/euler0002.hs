
nextFib :: (Integral a) => [a] -> [a]
nextFib [] = [0]
nextFib [0] = 1 : [0]
nextFib [_] = error "Incorrect list"
nextFib (a : (b : xs)) = (a + b) : a : b : xs

mysum :: (Integral n) => n -> [n] -> n
mysum 0 _ = 0
mysum n [] = mysum n (nextFib [])
mysum n (x:xs) = if x >= n then sum (filter even xs) else mysum n (nextFib (x:xs))

main = do
    print (mysum 4000000 [])
