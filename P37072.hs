data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

size :: Tree a -> Int
size Empty = 0
size (Node _ fe fd) = 1 + size fe + size fd

height :: Tree a -> Int
height Empty = 0
height (Node _ fe fd) = 1 + max (height fe) (height fd)

equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal (Node x fe fd) (Node y fey fdy) = (x==y) && (equal fe fey) && (equal fd fdy)
equal _ _ = False

isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic Empty Empty = True
isomorphic (Node x fe fd) (Node y fey fdy) = x==y && (cas1 || cas2)
    where
        cas1 = isomorphic fe fey && isomorphic fd fdy
        cas2 = isomorphic fe fdy && isomorphic fd fey
isomorphic _ _ = False

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node x fe fd) = [x] ++ preOrder fe ++ preOrder fd

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node x fe fd) = postOrder fe ++ postOrder fd ++ [x]

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node x fe fd) = inOrder fe ++ [x] ++ inOrder fd

breadthFirst :: Tree a -> [a]
breadthFirst a = bfs' [a] 
    where
        bfs' [] = []
        bfs' (Empty:as) = bfs' as
        bfs' ((Node x fe fd):as) = x : bfs' (as ++ [fe] ++ [fd])

        
build :: Eq a => [a] -> [a] -> Tree a
build [] [] = Empty
build (x:xs) y = Node x (build x1 yhead) (build x2 ytail)
    where yhead = takeWhile (/= x) y
          ytail = tail $ dropWhile (/= x) y
          x1 = take (length yhead) xs
          x2 = drop (length yhead) xs
          
overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
overlap op Empty x = x
overlap op x Empty = x
overlap op Empty Empty = Empty
overlap op (Node x fex fdx) (Node y fey fdy) = (Node (op x y) fe fd)
    where 
        fe = overlap op fex fey
        fd = overlap op fdx fdy


{-
let t7 = Node 7 Empty Empty
let t6 = Node 6 Empty Empty
let t5 = Node 5 Empty Empty
let t4 = Node 4 Empty Empty
let t3 = Node 3 t6 t7
let t2 = Node 2 t4 t5
let t1 = Node 1 t2 t3
let t1' = Node 1 t3 t2-}
