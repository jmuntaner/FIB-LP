insert :: [Int] -> Int -> [Int]
insert [] y = [y]

insert (x:xs) y
    | x >= y    = y : x : xs
    | x < y     = x : (insert xs y)

isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert (isort xs) x
    

remove :: [Int] -> Int -> [Int]

remove (x:xs) y
    | x == y    = xs
    | otherwise = x : (remove xs y)


ssort :: [Int] -> [Int]
ssort [] = []
ssort  x = z : (ssort (remove x z))
    where z = minimum x

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge [] z  = z
merge z []  = z

merge (x:xs) (y:ys)
    | x < y     = x : (merge xs (y:ys))
    | x == y    = x : y : (merge xs ys)
    | x > y     = y : (merge (x:xs) ys)

msort :: [Int] -> [Int]
msort [] = []
msort [a] = [a]
msort xs = merge (msort (fstHalf)) (msort (sndHalf))
    where   fstHalf = take (div n 2) xs        
            sndHalf = drop (div n 2) xs
            n = length xs

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = (qsort (divDown xs x)) ++ [x] ++ (qsort (divUp xs x))

divUp :: Ord a => [a] -> a -> [a]
divUp [] y = []

divUp (x:xs) y
    | x > y     = x : (divUp xs y)
    | otherwise = divUp xs y

divDown :: Ord a => [a] -> a -> [a]
divDown [] y = []

divDown (x:xs) y
    | x <= y     = x : (divDown xs y)
    | otherwise = divDown xs y

genQsort :: Ord a => [a] -> [a]
genQsort [] = []
genQsort (x:xs) = (genQsort (divDown xs x)) ++ [x] ++ (genQsort (divUp xs x))