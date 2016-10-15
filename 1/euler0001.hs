
mysum :: (Integral n) => n -> n
mysum n = sum [x | x <- [1..(n-1)], (mod x 3) == 0 || (mod x 5) ==0]

main = do
    print (mysum 1000)
