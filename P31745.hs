flatten :: [[Int]] -> [Int]
flatten x = foldr (++) [] x

myLength :: String -> Int
myLength x = sum $ map (const 1) x

myReverse :: [Int] -> [Int]
myReverse x = foldl (flip (:)) [] x

countIn :: [[Int]] -> Int -> [Int]
countIn x y = map (length . filter (== y)) x

firstWord :: String -> String
firstWord x = takeWhile aux2 $ dropWhile aux1 x

aux1 :: Char -> Bool
aux1 a = (a == ' ')

aux2 :: Char -> Bool
aux2 a = (a /= ' ')
