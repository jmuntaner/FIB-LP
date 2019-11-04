{--En aquest problema heu d’implementar una sèrie de funcions usant llistes per comprensió.

    Feu una funció myMap :: (a -> b) -> [a] -> [b] que emuli el map usant llistes per comprensió.
    Feu una funció myFilter :: (a -> Bool) -> [a] -> [a] que emuli el filter usant llistes per comprensió.
    Feu una funció myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c] que que emuli el zipWith usant llistes per comprensió i zip.
    Feu una funció thingify :: [Int] -> [Int] -> [(Int, Int)] que, donades dues llistes d’enters, genera la llista que aparella els elements si l’element de la segona llista divideix al de la primera.
    Feu una funció factors :: Int -> [Int] que, donat un natural no nul, genera la llista ordenada amb els seus factors (no necessàriament primers).--}
    
myMap :: (a -> b) -> [a] -> [b]
myMap f x  = [f y | y <- x]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p x = [y | y <- x, p y]

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f xs ys = [uncurry f x | x <- zip xs ys]

thingify :: [Int] -> [Int] -> [(Int, Int)]
thingify xs ys = [(x,y) | x <- xs, y <- ys, rem x y == 0]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], rem n x == 0]
