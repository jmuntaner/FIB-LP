absValue :: Int -> Int

absValue n
    | n>=0  = n
    | otherwise = (-n)
    
power :: Int -> Int -> Int

power x n
    | n==0  = 1
    | even n    = y * y
    | otherwise = y * y * x
    where
        y = power x (div n 2)

slowFib :: Int -> Int

slowFib n
    | n <= 1    = n
    | otherwise = slowFib (n-1) + slowFib (n-2);
    
quickFib :: Int -> Int
quickFib n = fst(quickFib' n)
    
-- quickFib' ret Fn Fn-1
quickFib' :: Int -> (Int, Int)
quickFib' 0 = (0,0)
quickFib' 1 = (1,0)
quickFib' n = (fn,fn1)
    where 
        (fn1, fn2) = quickFib' (n - 1)
        fn = fn1 + fn2

isPrime :: Int -> Bool
isPrime n
    | n < 2     = False
    | n <= 3    = True
    | otherwise = isPrime' n 2


isPrime' :: Int -> Int -> Bool
isPrime' a b 
    | b > 2 && b*b <= a  = ((mod a b) /= 0) && (isPrime' a (b + 2))
    | b == 2            = ((mod a b) /= 0) && (isPrime' a 3)
    | otherwise   = True


