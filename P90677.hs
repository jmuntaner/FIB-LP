{- Aquest problema explora la definició de funcions d’ordre superior sobre llistes. Implementeu les funcions següents mimetitzant les funcions originals de Haskell però sense usar la funció original (és a dir, no podeu usar foldl per implementar myFoldl però si per implementar myAll). A més, només podeu utilitzar recursivitat per definir myFoldl, myFoldr, myIterate, myUntil i myZip. -}

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl op i [] = i
myFoldl op i (x:xs) = myFoldl op (op i x) xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr op i [] = i
myFoldr op i (x:xs) = op x (myFoldr op i xs)

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil p f x
    | p x       = x
    | otherwise = myUntil p f (f x)

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:).f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (\x xs -> if p x then x:xs else xs) []

myAll :: (a -> Bool) -> [a] -> Bool
myAll f x = and $ map f x

myAny :: (a -> Bool) -> [a] -> Bool
myAny f x = or $ map f x

myZip :: [a] -> [b] -> [(a, b)]
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys
myZip [] y = []
myZip x [] = []

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f x y = map (uncurry f) (zip x y)
    
