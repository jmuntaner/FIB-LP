import Control.Applicative
import Control.Monad

data Queue a = Queue [a] [a]
     deriving (Show)
 
instance Eq a => Eq (Queue a) 
    where
        (Queue x1 y1) == (Queue x2 y2) = x1 ++ reverse y1 == x2 ++ reverse y2
        
instance Functor (Queue) where 
    fmap f (Queue x y) = Queue (map f x) (map f y)
    
instance Applicative (Queue) where
    pure a = (Queue [a] [])
    f <*> x = (Queue l [])
        where l = (unwrap f) <*> (unwrap x)
              
instance Monad (Queue) where
    return = pure
    f >>= x = (Queue l [])
        where l = (unwrap f) >>= (unwrap . x)
     
create :: Queue a
create = Queue [] []
 
push :: a -> Queue a -> Queue a
push x (Queue y z) = Queue y (x:z)
 
pop :: Queue a -> Queue a
pop (Queue (x:xs) y) = Queue xs y
pop (Queue [] y) = Queue (tail $ reverse y) []
 
top :: Queue a -> a
top (Queue (x:xs) y) = x
top (Queue [] y) = head $ reverse y
 
empty :: Queue a -> Bool
empty (Queue [] []) = True
empty _ = False

translation :: Num b => b -> Queue b -> Queue b
translation x q = fmap ((+) x) q

unwrap :: Queue a -> [a]
unwrap (Queue x y) = x ++ reverse y

kfilter :: (p -> Bool) -> Queue p -> Queue p
kfilter f q = do
    x <- q
    if (f x) then return x else create

