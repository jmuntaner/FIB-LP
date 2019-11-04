data Queue a = Queue [a] [a]
     deriving (Show)
 
instance Eq a => Eq (Queue a) 
    where
        (Queue x1 y1) == (Queue x2 y2) = x1 ++ reverse y1 == x2 ++ reverse y2
        
    
     
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
