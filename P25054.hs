myLength :: [Int] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myMaximum :: [Int] -> Int
myMaximum [a] = a
myMaximum (x:xs) = max x (myMaximum xs)


average :: [Int] -> Float
average xs = (fromIntegral (sum xs)) / (fromIntegral (length xs))

buildPalindrome :: [Int] -> [Int]
buildPalindrome x = reverse' x ++ x

--ineficient preguntar com fer eficient
reverse' :: [Int] -> [Int]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

remove :: [Int] -> [Int] -> [Int]
remove [] y = []

remove (x:xs) y
    | appears x y  = remove xs y
    | otherwise     = x : (remove xs y)

appears :: Int -> [Int] -> Bool
appears x (y:ys) = (x==y) || (appears x ys)
appears x [] = False

--  ????
flatten :: [[Int]] -> [Int]
flatten x = concat x

oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens [] = ([],[])

oddsNevens (x:xs)
    | even x    = (fst y, x : snd y)
    | odd x     = (x : fst y, snd y)
        where y = oddsNevens(xs)

oddsNevens' :: [Int] -> ([Int],[Int])
oddsNevens' x = ((removeEven x),(removeOdd x))

removeEven :: [Int] -> [Int]
removeEven [] = []

removeEven (x:xs)
    | even x  = removeEven xs
    | otherwise     = x : (removeEven xs)

removeOdd :: [Int] -> [Int]
removeOdd [] = []

removeOdd (x:xs)
    | odd x     = removeOdd xs
    | otherwise = x : (removeOdd xs)

primeDivisors :: Int -> [Int]
primeDivisors x = primeDiv 2 x

primeDiv :: Int -> Int -> [Int]

primeDiv i n
    | i <= limit && (mod n i == 0) && (isPrime i)   = i : (primeDiv (i+1) n)
    | i > limit     = []
    | otherwise     = primeDiv (i+1) n
        where limit = n

isPrime :: Int -> Bool

isPrime n
    | n < 2     = False
    | n <= 3    = True
    | otherwise = not (isPrime' n (ceiling (sqrt (fromIntegral n))))


isPrime' :: Int -> Int -> Bool

isPrime' a b 
    | b > 1     = ((mod a b) == 0) || (isPrime' a (b - 1))
    | otherwise    = False