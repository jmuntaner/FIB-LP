import Control.Monad

main :: IO ()
main = do
    nom <- getLine
    if ((last nom) == 'a' || (last nom) == 'A') then do
         putStrLn "Hola maca!"
    else do 
        putStrLn "Hola maco!"
    return ()