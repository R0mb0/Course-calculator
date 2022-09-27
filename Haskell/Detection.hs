module Detection where
import GHC.Float
import GHC.Num

{-**** Defining the functions to work with detections. ****-}
            
{-Get the longitude string part from the detection string.
* Input: A detection string.
* Output: A string without the latitude string part.-}
split :: String -> String
split = drop 17 

{-Verify if the detection string has the right lenght.
* Input: A detection string.
* Output: A boolean that is "True" if the detection string has the right lenght, "False" otherwise.-}
verifyLenght :: String -> Bool
verifyLenght [] = error "Null argument"
verifyLenght st
               | length st < 32 || length st > 32 = False
               | otherwise = True

{-Verify if the detection string is in the right format.
* Input: A detection string.
* Output: A boolean that is "True" if the detection string is in the right format, "False" otherwise.-}
verifyFormat :: String -> Bool
verifyFormat [] = error "Null argument"
verifyFormat st
               | not (verifyLenght st) = False
               | st !! 1 /= ' ' = False
               | st !! 4 /= ' ' = False
               | st !! 7 /= ' ' = False
               | st !! 14 /= ' ' = False
               | st !! 16 /= ' ' = False
               | st !! 18 /= ' ' = False
               | st !! 22 /= ' ' = False
               | st !! 25 /= ' ' = False
               | otherwise = True

{-Get the latitude tupla from the detection string.
* Input: A detection string.
* Output: A tupla containing the latitude in D.M.G. format,
   (sign, degrees, primes, seconds).-}
getLatitude :: String -> (Char, Int, Int, Float)
getLatitude [] = error "Null argument"
getLatitude st
              | not (verifyFormat st) = error ("Invalid argument: " ++ st)
              | otherwise = (head st, read (take 2 (drop 2 st)) :: Int, read (drop 5 (take 7 st)) :: Int, read (drop 8 (take 14 st)) :: Float)

{-Get the longitude tupla from the detection string.
* Input: A detection string.
* Output: A tupla containing the longitude in D.M.G. format,
   (sign, degrees, primes, seconds).-}
getLongitude :: String -> (Char, Int, Int, Float)
getLongitude [] = error "Null argument"
getLongitude st
               | not (verifyFormat st) = error ("Invalid argument: " ++ st)
               | otherwise = (head (split st), read (take 3 (drop 2 (split st))) :: Int, read (drop 6 (take 8 (split st))) :: Int, read (drop 9 (take 15 (split st))) :: Float)

{-** Verify if latitude & longitude are real. **-}

{-Verify if the latitude degrees are real.
 * Input: A latitude tupla.
 * Output: A boolean that is "True" if the latitude degrees are real. "False" otherwise.-}
verifyLatDeg :: (Char, Int, Int, Float) -> Bool 
verifyLatDeg (s, x, y, z)
                         | x < 0 || x > 89 = error ("Wrong degree in: " ++ pt)
                         | otherwise = True
                         where
                              pt = " " ++ show s ++ " " ++ show x ++ " " ++ show y++ " " ++ show z

{-Verify if the longitude degrees are real.
 * Input: A longitude tupla.
 * Output: A boolean that is "True" if the longitude degrees are real. "False" otherwise.-}
verifyLongDeg :: (Char, Int, Int, Float) -> Bool 
verifyLongDeg (s, x, y, z)
                          | x < 0 || x > 179 = error ("Wrong degree in: " ++ pt)
                          | otherwise = True
                          where
                               pt = " " ++ show s ++ " " ++ show x ++ " " ++ show y++ " " ++ show z

{-Verify the latitude/longitude tupla body,
   (In this case the degrees of the body aren't valutated).
 * Input: A latitude or longitude tupla.
 * Output: A boolean that is "True" if the primes & seconds are real. False otherwise.-}
verifyDetBody :: (Char, Int, Int, Float) -> Bool
verifyDetBody (s, x, y, z)
                          | y < 0 || y > 59 = error ("Wrong primes in: " ++ pt)
                          | z < 0 || z > 59 = error ("Wrong seconds in: " ++ pt)
                          | otherwise = True 
                          where
                              pt = " " ++ show s ++ " " ++ show x ++ " " ++ show y++ " " ++ show z

{-Verify if the latitude sign is right.
 * Input: A latitude tupla.
 * Output: A boolean that is "True" if the latitude sign is right. "False" otherwise.-}
verifyLatSign :: (Char, Int, Int, Float) -> Bool 
verifyLatSign (s, x, y, z)
                          | s == 'N' || s == 'S' = True
                          | otherwise = error ("Wrong sign in: " ++ pt)
                          where 
                              pt = " " ++ show s ++ " " ++ show x ++ " " ++ show y++ " " ++ show z

{-Verify if the longitude sign is right.
 * Input: A longitude tupla.
 * Output: A boolean that is "True" if the longitude sign is right. "False" otherwise.-}
verifyLongSign :: (Char, Int, Int, Float) -> Bool 
verifyLongSign (s, x, y, z)
                           | s == 'E' || s == 'W' = True
                           | otherwise = error ("Wrong sign in: " ++ pt)
                           where 
                               pt = " " ++ show s ++ " " ++ show x ++ " " ++ show y++ " " ++ show z

{-Verify the latitude coordinate.
* Input: A latitude tupla.
* Output: A boolean that is "True" if the coordinate is real. "False" otherwise.-}
verifyLat :: (Char, Int, Int, Float) -> Bool 
verifyLat (s, x, y, z)  
                      | verifyDetBody (s,x,y,z) && verifyLatDeg (s,x,y,z) && verifyLatSign (s,x,y,z) = True
                      | otherwise = False

{-Verify the longitude coordinate.
* Input: A longitude tupla.
* Output: A boolean that is "True" if the coordinate is real. "False" otherwise.-}
verifyLong :: (Char, Int, Int, Float) -> Bool 
verifyLong (s, x, y, z)  
                       | verifyDetBody (s,x,y,z) && verifyLongDeg (s,x,y,z) && verifyLongSign (s,x,y,z) = True
                       | otherwise = False
                     
{-Convert the latitude/longitude in D.M.G. format to decimal format.
* Input: A latitude or longitude tupla.
* Output: A double number containing the latitude/longitude in decimal format.-}
convertToDecimal :: (Char, Int, Int , Float ) -> Double  
convertToDecimal (s, x, y, z) 
                             | s == 'S' || s == 'W' = float2Double (((((z / 60) + fromIntegral y) / 60) + fromIntegral x) * (-1))
                             | otherwise = float2Double ((((z / 60) + fromIntegral y) / 60) + fromIntegral x)

{-Merge the latitude & longitude coordinates in a List.
* Input: Two elements of the same type.
* Output: A list containing the input elements.-}
merge :: a -> a -> [a]
merge lat long = [lat, long]

{-Transform the detection string into a list containing the latitude & the longitude in decimal format.
* Input: A detection string.
* Output: A list of doubles containing the latitude & the longitude in decimal format.-}
getPoint :: String -> [Double]
getPoint [] = error "Null argument"
getPoint st = if verifyLat (getLatitude st) && verifyLong (getLongitude st) 
              then merge (convertToDecimal (getLatitude st)) (convertToDecimal (getLongitude st))
              else []