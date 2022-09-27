import Detection
import Properties
import Tools

{-Verify if the two detections inserted are different.
* Input: Two strings.
* Output An error if the two detections inserted aren't different, "true" otherwise.-}
verifyDetections :: String -> String -> Bool
verifyDetections detA detB 
                          | detA == detB = error "Inserted the same detection twice"
                          | otherwise = True

{-A function to print the course properties.
* Input: The strings of the two detections required from the main.
* Output: Print the course properties.-}
printInfo :: String -> String -> IO ()
printInfo [] [_] = error "The first argument is null"
printInfo [_] [] = error "The second argument is null"
printInfo detA detB = do
    putStr "First detection in decimal format ---> "
    putStrLn (show (round3dp (head (getPoint detA)))++ "," ++ show (round3dp (getPoint detA !! 1)))
    putStr "Second detection in decimal format ---> "
    putStrLn (show (round3dp (head (getPoint detB)))++ "," ++ show (round3dp (getPoint detB !! 1)))
    putStr "Distance between first & second detections ---> "
    putStrLn (show (round2dp (distance (getPoint detA) (getPoint detB))) ++ "Km")
    putStr "Positive direction between first & second detections ---> "
    putStrLn (show (round2dp (direction (getPoint detA) (getPoint detB))) ++ "°")
    putStr "Negative direction between first & second detections ---> "
    putStrLn (show (round2dp (invDirection (getPoint detA) (getPoint detB))) ++ "°")
    
{-Main.-}
main :: IO ()
main = do
    putStrLn "Detections Properties Calculator V1.0 \nWarning: the detections must be in D.M.G \
    \format and inserted into the program like: N 40 45 36.000 - E 073 59 02.400"
    putStrLn "Insert the first detection..."
    detA <- getLine
    putStrLn "Insert the second detection..."
    detB <- getLine 
    putStrLn "Proceed [yes/no]?"
    answ <- getLine 
    if answ  == "yes" && verifyDetections detA detB
        then printInfo detA detB
        else putStrLn "Aborted..."