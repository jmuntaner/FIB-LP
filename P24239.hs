import Control.Monad

arrels :: Float -> [Float]
arrels x = x : (next x x)
    where next x y = z : (next x z)
            where z = (y+x/y)/2

arrel :: Float -> Float -> Float
arrel x e = fst $ head $ dropWhile (cond e) $ zip (tail l) l
    where l = arrels x
          cond e u = abs(a-b) > e
            where a = fst u
                  b = snd u

roman2int :: String -> Int
roman2int [] = 0
roman2int [a] = val a
roman2int (a:b:xs)
    | val(a) < val(b) = (-val(a)) + roman2int (b:xs)
    | otherwise       = val(a) + roman2int (b:xs)

val :: Char -> Int
val 'I' = 1
val 'V' = 5
val 'X' = 10
val 'L' = 50
val 'C' = 100
val 'D' = 500
val 'M' = 1000

roman2int' :: String -> Int
roman2int' [] = 0
roman2int' [a] = val a
roman2int' x = sum (zipWith f x ((tail x) ++ [(last x)]))
    where f x1 x2 = if ((val x1) < (val x2)) then - (val x1) else (val x1)
    
data LTree a = Leaf a | Node (LTree a) (LTree a)

instance (Show a) => Show (LTree a) where
        show (Leaf a) = "{" ++ show a ++ "}"
        show (Node ta tb) = "<" ++ (show ta) ++ "," ++ (show tb) ++ ">" 
        
build :: [a] -> LTree a
build [x] = (Leaf x)
build xs = Node (build x) (build y)
    where x = take (div (length xs + 1) 2) xs
          y = drop (div (length xs + 1) 2) xs
          
zipLTrees :: LTree a -> LTree b -> Maybe (LTree (a,b))
zipLTrees (Leaf x) (Leaf y) = do {return (Leaf (x, y))}
zipLTrees (Node t1 t2) (Node t3 t4) = do {tx <- zipLTrees t1 t3; ty <- zipLTrees t2 t4; return (Node tx ty)}
zipLTrees _ _ = do Nothing
