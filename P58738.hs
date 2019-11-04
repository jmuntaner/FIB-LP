import Control.Monad

data STree a = Nil | Node Int a (STree a) (STree a) deriving Show

instance Functor (STree) where
    fmap f Nil = Nil
    fmap f (Node y x t1 t2) = Node y (f x) (fmap f t1) (fmap f t2)

isOk :: STree a -> Bool
isOk (Nil) = True
isOk n@(Node x _ t1 t2) = (isOk t1) && (isOk t2) && (count n == x)
    where 
        count (Nil) = 0
        count (Node x _ t1 t2) = 1 + count t1 + count t2

nthElement :: STree a -> Int -> Maybe a
nthElement Nil _ = Nothing
nthElement (Node t x t1 t2) n
    | n == 0    = Nothing
    | n > t     = Nothing
    | n == (c1+1)   = Just x
    | n <= c1       = nthElement t1 n
    | otherwise     = nthElement t2 (n-(c1+1))
    where c1 = f t1
          f (Node t _ _ _) = t
          f (Nil) = 0

mapToElements :: (a -> b) -> STree a -> [Int] -> [Maybe b]
mapToElements f t l = map (liftM f) list
    where list = map (nthElement t) l