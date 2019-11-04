-- Donat un predicat sobre els enters i una llista d’enters, retorna el nombre d’elements de la llista que satisfan el predicat.
countIf :: (Int -> Bool) -> [Int] -> Int
countIf p x = length $ filter p x

-- Donada una llista d’enters i una llista de funcions d’enters a enters, 
-- retorna la llista de llistes resultant d’aplicar cada una de les funcions de la segona llista als elements de la primera llista.
pam :: [Int] -> [Int -> Int] -> [[Int]]
pam x = foldr ((:).(flip map x)) []

pam2 :: [Int] -> [Int -> Int] -> [[Int]]
pam2 x f = foldr ((:).(flip apli f)) [] x

--pam2 [] f = []
--pam2 (x:xs) f = (apli x f) : (pam2 xs f)

apli :: Int -> [Int -> Int] -> [Int]
apli x = foldr ((:).(flip ($) x)) []

--apli x [] = []
--apli x (f:fs) = (f x) : (apli x fs)


-- pam2 x:xs = foldr ((:).(flip map x)) []

-- Donada una llista d’enters i una llista de funcions d’enters a enters, 
-- retorna la llista de llistes on cada llista és el resultat d’aplicar successivament les funcions de la segona llista a cada element de la primera llista.
--Nota: Qualsevol semblança amb La parte contratante de la primera parte será considerada como la parte contratante de la primera parte és pura casualitat.

filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterFoldl p op ini x = foldl op ini (filter p x)

-- Que donada una relació entre enters, una llista i un element, ens retorna la llista amb l’element inserit segons la relació.

insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int]
insert cmp l y = fst p1 ++ [y] ++ snd p1
    where p1 = (filter (flip cmp y) l, filter (not . (flip cmp y)) l)

--Utilitzant la funció insert, feu una funció que ordeni la llista per inserció segons la relació donada.
insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int]
insertionSort cmp = foldl (insert cmp) []