fib :: Int -> Integer
fib 0 = 0
fib n = fst (fib2 (n-1))

-- | Return (fib n, fib (n + 1))
fib2 0 = (1, 1)
fib2 1 = (1, 2)
fib2 n
 | even n    = (a*a + b*b, c*c - a*a)
 | otherwise = (c*c - a*a, b*b + c*c)
 where (a,b) = fib2 (n `div` 2 - 1)
       c     = a + b