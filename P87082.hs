import System.IO
import Control.Monad

imc :: Float -> Float -> Float
imc m h = m/(h*h)

getWord :: IO String
getWord = do
            c <- getChar;
            if (c == '\n' || c == ' ')
            then return ("")
            else do {w <- getWord; return (c:w)}
            
            

interpretacio :: Float -> String
interpretacio x
    | x < 18    = "magror"
    | x < 25    = "corpulencia normal"
    | x < 30    = "sobrepes"
    | x < 40    = "obesitat"
    | otherwise = "obesitat morbida"

main :: IO ()
main =  do 
            nom <- getWord
            if nom == "*" then return ()
            else do
                m <- getWord
                h <- getWord
                let i = imc (read m) (read h)
                putStr nom
                putStr ": "
                putStrLn (interpretacio i)
                main