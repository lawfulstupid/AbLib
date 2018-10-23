split :: [a] -> ([a], [a])
split [] = ([], [])
split (a:[]) = ([a], [])
split (a:b:l) = let (x, y) = split l
                in (a:x, b:y)

minmax :: (Integral a) => [a] -> (a, a)
minmax [x] = (x, x)
minmax l = let (x, y) = split l
               (xl, xh) = minmax x
               (yl, yh) = minmax y
           in (min xl yl, max xh yh)

gcd :: (Integral a) => [a] -> a
gcd [a] = a
gcd [a,b] = let (x,y) = minmax [a,b]
            in 
gcd l = let (a, b) = fmap gcd $ split l
        in gcd [a,b]