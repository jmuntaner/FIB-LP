sumMultiples35 :: Integer -> Integer
sumMultiples35 n = (3*x+5*y) - (15*z)
    where
        nats = iterate (+ 1) 1
        x  = div (xl * (xl+1)) 2
        y  = div (yl * (yl+1)) 2
        z  = div (zl * (zl+1)) 2
        xl = div (n-1) 3
        yl = div (n-1) 5
        zl = div (n-1) 15

fibonacci :: Int -> Integer
fibonacci n = fibs !! n
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

sumEvenFibonaccis :: Integer -> Integer
sumEvenFibonaccis n = sum $ takeWhile (<n) $ filter (even) fibs
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

largestPrimeFactor :: Int -> Int
largestPrimeFactor 1 = 1
largestPrimeFactor n = largest n 2 (-1)

-- n i max
largest :: Int -> Int -> Int -> Int
largest n i m
        | i*i > n && n > 2  = n
        | (mod n i) == 0    = largest (div n i) i i
        | i == 2            = largest n 3 m
        | i*i > n           = m
        | otherwise         = largest n (i+2) m


isPalindromic :: Integer -> Bool
isPalindromic x = (show x) == reverse (show x)
