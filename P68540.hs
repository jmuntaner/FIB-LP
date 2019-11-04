sumDigits :: Integer -> Integer
sumDigits x = sum $ (digits x)
    where digits = map (read . (:[])) . show

tartaglia :: [[Integer]]
tartaglia = iterate tarNext [1]
    where tarNext x = zipWith (+) (0:x) (x++[0])

diffSqrs :: Integer -> Integer
diffSqrs n = a - b
    where a = div (n*n*(n+1)*(n+1)) 4
          b = div (n*(n+1)*(2*n+1)) 6

digitalRoot :: Integer -> Integer
digitalRoot x = head $ dropWhile (>9) $ iterate (sumDigits) x

pythagoreanTriplets :: Int -> [(Int, Int, Int)]
pythagoreanTriplets n = [(x,y,z) | x<-[1..n], y<-[x..n], z<-[y..n], x+y+z==n, x*x+y*y==z*z]