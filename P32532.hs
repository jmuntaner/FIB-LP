divisors :: Int -> [Int]
divisors n = filter (\x->(mod n x == 0)) [1..n]

nbDivisors :: Int -> Int
nbDivisors = length . divisors

moltCompost :: Int -> Bool
moltCompost 1 = True
moltCompost 2 = True
moltCompost n = and l
    where l = [y > (nbDivisors x) | x <- [1..(n-1)]]
          y = nbDivisors n