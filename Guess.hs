import System.Random

main :: IO ()
main = do
     --cast using ::
     number <- randomRIO(1, 100) :: IO Int
     runGame number 0

runGame :: Int -> Int -> IO ()
runGame number tries = do
    putStr "Enter a number "
    let t = tries + 1
    x <- getLine
    --cast via read and :: 
    let guess = read x :: Int
    case compare guess number of 
    	 LT -> putStrLn "To low" 
    	 GT -> putStrLn "To high"
    	 EQ -> putStrLn("Great! You used " ++ show t ++ " tries") 
    if guess /= number then
       runGame number t	 	       		
    else return ()      
        