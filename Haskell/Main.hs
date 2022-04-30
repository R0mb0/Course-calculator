import Detection
import Properties
import Tools

printInfo :: String -> String -> IO ()
printInfo [] [_] = error "The First Argument Is Null"
printInfo [_] [] = error "The Second Argument Is Null"
printInfo detA detB
                   | length detA < 31 || length detA > 31 = error "The First Argument Is Invalid"
                   | length detB < 31 || length detB > 31 = error "The Second Argument Is Invalid"
                   |otherwise = do
    putStr "First Detection in Decimal Format ---> "
    putStrLn (show (round5dp (head (getPoint detA)))++ "," ++ show (round5dp (getPoint detA !! 1)))
    putStr "Second Detection in Decimal Format ---> "
    putStrLn (show (round5dp (head (getPoint detB)))++ "," ++ show (round5dp (getPoint detB !! 1)))
    putStr "Distance between First & Second Detections ---> "
    putStrLn (show (round2dp (distance (getPoint detA) (getPoint detB))) ++ "Km")
    putStr "Positive direction between First & Second Detections ---> "
    putStrLn (show (round2dp(direction (getPoint detA) (getPoint detB))) ++ "°")
    putStr "Negative direction between First & Second Detections ---> "
    putStrLn (show (round2dp(invDirection (getPoint detA) (getPoint detB))) ++ "°")
    
main :: IO ()
main = do
    putStrLn "Detections Properties Calculator V1.0 \n Waring: The Detections must be in D.M.G \
    \format and inserted into the program like: N 40 45 36.000 - E 73 59 2.400"
    putStrLn "Insert the First Detection..."
    detA <- getLine
    verStr detA
    putStrLn "Insert the Second Detection..."
    detB <- getLine 
    verStr detB
    putStrLn "Proceed [Y/n] ?"
    answ <- getChar 
    getLine {-In way to press enter-}
    if answ  == 'Y'
        then printInfo detA detB
        else putStrLn "Aborted..."