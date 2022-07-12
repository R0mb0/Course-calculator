module Detection where
import GHC.Float
import GHC.Num

{-**** Defining the Functions to Work with Detections. ****-}
            
{-Get the Longitude string part from the Detection string.
* Input: A Detection String.
* Output: A String without the Latitude string part.-}
split :: String -> String
split = drop 17 

{-Verify if the Detection string Has the Right Lenght.
* Input: A Detection String.
* Output: A Boolean that is "True" If the Detection string Has the Right Lenght, "False" Otherwise.-}
verifyLenght :: String -> Bool
verifyLenght [] = error "Null Argument"
verifyLenght st
               | length st < 32 || length st > 32 = False
               | otherwise = True

{-Verify if the Detection string is in the Right Format.
* Input: A Detection String.
* Output: A Boolean that is "True" If the Detection string is in the Right Format, "False" Otherwise.-}
verifyFormat :: String -> Bool
verifyFormat [] = error "Null Argument"
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

{-Get the Latitude Tupla from the Detection string.
* Input: A Detection String.
* Output: A Tupla Containing the Latitude in D.M.G format,
   (Sign, Degrees, Primes, Latters).-}
getLatitude :: String -> (Char, Int, Int, Float)
getLatitude [] = error "Null Argument"
getLatitude st
              | not (verifyFormat st) = error ("Invalid Argument: " ++ st)
              | otherwise = (head st, read (take 2 (drop 2 st)) :: Int, read (drop 5 (take 7 st)) :: Int, read (drop 8 (take 14 st)) :: Float)

{-Get the Longitude Tupla from the Detection string.
* Input: A Detection String.
* Output: A Tupla Containing the Longitude in D.M.G format,
   (Sign, Degrees, Primes, Latters).-}
getLongitude :: String -> (Char, Int, Int, Float)
getLongitude [] = error "Null Argument"
getLongitude st
               | not (verifyFormat st) = error ("Invalid Argument: " ++ st)
               | otherwise = (head (split st), read (take 3 (drop 2 (split st))) :: Int, read (drop 6 (take 8 (split st))) :: Int, read (drop 9 (take 15 (split st))) :: Float)

{-** Verify if Latitude & Longitude are Real. **-}

{-Verify if the Latitude Degrees are Real.
 * Input: A Latitude Tupla.
 * Output: A Boolean that is "True" if the Latitude Degrees are Real. "False" otherwise.-}
verifyLatDeg :: (Char, Int, Int, Float) -> Bool 
verifyLatDeg (s, x, y, z)
                         | x < 0 || x > 89 = error ("Wrong Degree in: " ++ pt)
                         | otherwise = True
                         where
                              pt = " " ++ show s ++ " " ++ show x ++ " " ++ show y++ " " ++ show z

{-Verify if the Longitude Degrees are Real.
 * Input: A Longitude Tupla.
 * Output: A Boolean that is "True" if the Longitude Degrees are Real. "False" otherwise.-}
verifyLongDeg :: (Char, Int, Int, Float) -> Bool 
verifyLongDeg (s, x, y, z)
                          | x < 0 || x > 179 = error ("Wrong Degree in: " ++ pt)
                          | otherwise = True
                          where
                               pt = " " ++ show s ++ " " ++ show x ++ " " ++ show y++ " " ++ show z

{-Verify the Latitude/Longitude Tupla Body,
   (In this case the degrees of the body aren't valutated).
 * Input: A Latitude or Longitude Tupla.
 * Output: A Boolean that is "True" if the Primes & Latters are Real. False otherwise.-}
verifyDetBody :: (Char, Int, Int, Float) -> Bool
verifyDetBody (s, x, y, z)
                          | y < 0 || y > 59 = error ("Wrong Primes in: " ++ pt)
                          | z < 0 || z > 59 = error ("Wrong Seconds in: " ++ pt)
                          | otherwise = True 
                          where
                              pt = " " ++ show s ++ " " ++ show x ++ " " ++ show y++ " " ++ show z

{-Verify if the Latitude Sign is Right.
 * Input: A Latitude Tupla.
 * Output: A Boolean that is "True" if the Latitude Sign is Right. "False" otherwise.-}
verifyLatSign :: (Char, Int, Int, Float) -> Bool 
verifyLatSign (s, x, y, z)
                          | s == 'N' || s == 'S' = True
                          | otherwise = error ("Wrong Sign in: " ++ pt)
                          where 
                              pt = " " ++ show s ++ " " ++ show x ++ " " ++ show y++ " " ++ show z

{-Verify if the Longitude Sign is Right.
 * Input: A Longitude Tupla.
 * Output: A Boolean that is "True" if the Longitude Sign is Right. "False" otherwise.-}
verifyLongSign :: (Char, Int, Int, Float) -> Bool 
verifyLongSign (s, x, y, z)
                           | s == 'E' || s == 'W' = True
                           | otherwise = error ("Wrong Sign in: " ++ pt)
                           where 
                               pt = " " ++ show s ++ " " ++ show x ++ " " ++ show y++ " " ++ show z

{-Verify the Latitude Coordinate.
* Input: A Latitude Tupla.
* Output: A Boolean that is "True" if the Coordinate is Real. "False" otherwise.-}
verifyLat :: (Char, Int, Int, Float) -> Bool 
verifyLat (s, x, y, z)  
                      | verifyDetBody (s,x,y,z) && verifyLatDeg (s,x,y,z) && verifyLatSign (s,x,y,z) = True
                      | otherwise = False

{-Verify the Longitude Coordinate.
* Input: A Longitude Tupla.
* Output: A Boolean that is "True" if the Coordinate is Real. "False" otherwise.-}
verifyLong :: (Char, Int, Int, Float) -> Bool 
verifyLong (s, x, y, z)  
                       | verifyDetBody (s,x,y,z) && verifyLongDeg (s,x,y,z) && verifyLongSign (s,x,y,z) = True
                       | otherwise = False
                     
{-Convert the Latitude/Longitude in D.M.G format to Decimal format.
* Input: A Latitude or Longitude Tupla.
* Output: A Double Number Containing the Latitude/Longitude in Decimal format.-}
convertToDecimal :: (Char, Int, Int , Float ) -> Double  
convertToDecimal (s, x, y, z) 
                             | s == 'S' || s == 'W' = float2Double (((((z / 60) + fromIntegral y) / 60) + fromIntegral x) * (-1))
                             | otherwise = float2Double ((((z / 60) + fromIntegral y) / 60) + fromIntegral x)

{-Merge the Latitude & Longitude coordinates in a List.
* Input: Two Elements of the Same Type.
* Output: A List Containing the Input Elements.-}
merge :: a -> a -> [a]
merge lat long = [lat, long]

{-Transform the Detection String into a List Containing the Latitude & the Longitude in Decimal format.
* Input: A Detection String.
* Output: A List of Doubles Containing the Latitude & the Longitude in Decimal format.-}
getPoint :: String -> [Double]
getPoint [] = error "Null Argument"
getPoint st = if verifyLat (getLatitude st) && verifyLong (getLongitude st) 
              then merge (convertToDecimal (getLatitude st)) (convertToDecimal (getLongitude st))
              else []