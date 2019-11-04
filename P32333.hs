fact1 :: Integer -> Integer
fact1 0 = 1
fact1 1 = 1
fact1 n = n * (fact1 (n-1))

fact2 :: Integer -> Integer
fact2 n = foldl (*) 1 [1..n]

fact3 :: Integer -> Integer
fact3 n = product [1..n]

fact4 :: Integer -> Integer
fact4 n
    | n < 2 = 1
    | otherwise = n * (fact4 (n-1))

fact5 :: Integer -> Integer
fact5 n = if n == 0 
           then 1
           else n * fact5 (n-1)

fact6 :: Integer -> Integer
fact6 n = facs !! (fromInteger n)
    where facs = scanl (*) 1 [1..]

fact7 :: Integer -> Integer
fact7 n = foldr (*) 1 [1..n]

fact8 :: Integer -> Integer
fact8 n = snd (until ((>n) . fst) (\(i,m) -> (i+1, i*m)) (1,1))
