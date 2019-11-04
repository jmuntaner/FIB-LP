import Data.List

zerosNones1 :: Int -> [[Int]]
zerosNones1 0 = [[]]
zerosNones1 n = map (0:) petits ++ map (1:) petits
    where
        petits = zerosNones1 (n-1)
        
zerosNones2 :: Int -> Int -> [[Int]]
zerosNones2 0 0 = [[]]
zerosNones2 n u 
    | u < 0 || u > n  = []
    | otherwise = map (0:) petits0 ++ map (1:) petits1
    where
        petits0 = zerosNones2 (n-1) u
        petits1 = zerosNones2 (n-1) (u-1)
        
subsets1 :: [a] -> [[a]]
subsets1 [] = [[]]
subsets1 (a:xs) = map (a:) resta ++ resta
    where
        resta = subsets1 xs
        
subsets2 :: Int -> [a] -> [[a]]
subsets2 0 _ = [[]]
subsets2 _ [] = []
subsets2 m (a:xs)
    | m == (length xs) + 1 = [(a:xs)]
    | otherwise = map (a:) resta1 ++ resta0
    where
        resta0 = subsets2 m xs
        resta1 = subsets2 (m-1) xs

    {-
0 0 0
0 0 1
0 1 0
0 1 1
1 0 0
1 1 0
1 0 1
1 1 1
-}
