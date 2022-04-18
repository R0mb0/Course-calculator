import Detection
import Properties
import Tools

printInfo :: String -> String -> IO ()
printInfo det1 det2 = do
    putStr "First Detection in Decimal Format ---> "
    putStrLn (show (round6dp (head (getPoint det1)))++ "," ++ show (round6dp (getPoint det1 !! 1)))
    putStr "Second Detection in Decimal Format ---> "
    putStrLn (show (round6dp (head (getPoint det2)))++ "," ++ show (round6dp (getPoint det2 !! 1)))
    putStr "Distance between First & Second Detections ---> "
    putStrLn (show (round (distance (getPoint det1) (getPoint det2)) :: Integer) ++ "Km")
    putStr "Positive direction between First & Second Detections ---> "
    putStrLn (show (round (direction (getPoint det1) (getPoint det2)) :: Integer) ++ "°")
    putStr "Negative direction between First & Second Detections ---> "
    putStrLn (show (round (invDirection (getPoint det1) (getPoint det2)) :: Integer) ++ "°")
    
main :: IO ()
main = do
    putStrLn "Detections Properties Calculator V1.0 \n Waring: The Detections must be in D.M.G \
    \format and inserted into the program like: N 40 45 36.000 - E 73 59 2.400"
    putStrLn "Insert the First Detection..."
    det1 <- getLine
    verStr det1
    putStrLn "Insert the Second Detection..."
    det2 <- getLine 
    verStr det2
    putStrLn "Proceed [Y/n] ?"
    answ <- getChar 
    {-In way to press enter-}
    getLine 
    if answ  == 'Y'
        then printInfo det1 det2
        else putStrLn "Aborted..."