import Detection
import Tools

printInfo :: String -> String -> IO ()
printInfo det1 det2 = do
    putStrLn "---> First Detection in Decimal Format..."
    print (getPoint det1)
    putStrLn "---> Second Detection in Decimal Format..."
    print (getPoint det2)
    putStrLn "---> Distance between First & Second Detections..."
    print (distance (getPoint det1) (getPoint det2))
    putStrLn "---> Positive direction between First & Second Detections..."
    print (direction (getPoint det1) (getPoint det2))
    putStrLn "---> Negative direction between First & Second Detections..."
    print (invDirection (getPoint det1) (getPoint det2))
    putStrLn "************** END **************"

main :: IO ()
main = do
    putStrLn "Detections Properties Calculator V1.0 \n Waring: The Detections must be in D.M.G \
    \format and inserted into the program like: N 40 45 36.000 - E 73 59 2.400"
    putStrLn "Insert the First Detection..."
    det1 <- getLine
    putStrLn "Coordinates Inserted:"
    print det1
    putStrLn "Insert the Second Detection..."
    det2 <- getLine 
    putStrLn "Coordinates Inserted:"
    print det2
    putStrLn "Proceed [Y/n] ?"
    answ <- getChar 
    if answ  == 'Y'
        then printInfo det1 det2
        else putStrLn "Aborted..."
    
{- N 43 54 16.000 - E 12 54 30.000 -}
{- N 43 54 17.000 - E 12 54 40.000 -}