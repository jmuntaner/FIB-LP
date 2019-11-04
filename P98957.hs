ones :: [Integer]
ones = iterate id 1

nats :: [Integer] 
nats = iterate (+ 1) 0

ints :: [Integer]
ints = 0 : (concat $ map aux (zip (tail nats) (map (* (-1)) $ tail nats)))

aux :: (Integer, Integer) -> [Integer]
aux x = (fst x) : (snd x) : []

triangulars :: [Integer]
triangulars = scanl1 (+) (nats)

factorials :: [Integer]
factorials = scanl (*) 1 (tail nats)

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

primes :: [Integer]
primes = garb (iterate (+ 1) 2)
    where 
        garb (p : xs) = p : garb [x | x <- xs, mod x p /= 0]
        
hammings :: [Integer]
hammings = 1 : merge3 (map (*2) hammings) (map (*3) hammings) (map (*5) hammings)

merge3 :: [Integer] -> [Integer] -> [Integer] -> [Integer]
merge3 xs ys zs = merge xs (merge ys zs)

merge :: [Integer] -> [Integer] -> [Integer]
merge (x:xs) (y:ys) 
    | (x==y)    = x : merge xs ys
    | x < y     = x : merge xs (y:ys)
    | x > y     = y : merge (x:xs) ys

lookNsay :: [Integer]
lookNsay = iterate (apli) 1

apli :: Integer -> Integer
apli x = digsInv (reverse $ next (digs x))

next :: [Integer] -> [Integer]
next [] = []
next (x:xs) = (toInteger $ length $ takeWhile ((==) x) (x:xs)) : x : (next $ dropWhile ((==) x) xs)

digs :: Integer -> [Integer]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

digsInv :: [Integer] -> Integer
digsInv [] = 0
digsInv (x:xs) = x + 10 * (digsInv xs)

tartaglia :: [[Integer]]
tartaglia = iterate (tarNext) [1]

tarNext :: [Integer] -> [Integer]
tarNext x = zipWith (+) (0:x) (x ++ [0])

