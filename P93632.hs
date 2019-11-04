eql :: [Int] -> [Int] -> Bool
eql x y = (and $ zipWith (==) x y) && (length x == length y)

prod :: [Int] -> Int
prod x = foldl (*) 1 x

prodOfEvens :: [Int] -> Int
prodOfEvens x = foldl (*) 1 $ filter even x

powersOf2 :: [Int]
powersOf2 = iterate (*2) 1

scalarProduct :: [Float] -> [Float] -> Float
scalarProduct x y = (sum $ zipWith (*) x y)
