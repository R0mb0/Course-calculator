module Detection where
import GHC.Float
import GHC.Num

{-**** Defining the Functions to Work with Detections. ****-}
            
{- Get the Longitude string part from the Detection string.
* Input: A Detection String.
* Output: A String without the Latitude string part.-}
split :: String -> String
split = drop 17 

{-Get the Latitude Tupla from the Detection string.
* Input: A Detection String.
* Outout: A Tupla Containing the Latitude in D.M.G format,
   (Sign, Degrees, Primes Latters).-}
getLatitude :: String -> (Char, Int, Int, Float)
getLatitude [] = error "Null Argument"
getLatitude st
              | length st < 31 || length st > 31 = error ("Invalid Argument: " ++ st)
              | otherwise = (head st, read (take 2 (drop 2 st)) :: Int, read (drop 5 (take 7 st)) :: Int, read (drop 8 (take 14 st)) :: Float)

{-Get the Longitude Tupla from the Detection string.
* Input: A Detection String.
* Outout: A Tupla Containing the Longitude in D.M.G format,
   (Sign, Degrees, Primes Latters).-}
getLongitude :: String -> (Char, Int, Int, Float)
getLongitude [] = error "Null Argument"
getLongitude st
               | length st < 31 || length st > 31 = error ("Invalid Argument: " ++ st)
               | otherwise = (head (split st), read (take 2 (drop 2 (split st))) :: Int, read (drop 5 (take 7 (split st))) :: Int, read (drop 8 (take 14 (split st))) :: Float)

{-** Verify if Latitude & Longitude are Real. **-}

{-Verify the Detection Latitude/Longitude Tupla Body.
 * Input: A Latitude or Longitude Tupla.
 * Output: True if the Degrees, Primes & Latters are Real. False otherwise.-}
verDetBody :: (Char, Int, Int, Float) -> Bool
verDetBody (s, x, y, z)
                       | x < 0 || x > 89 = error ("Wrong Degrees in: " ++ pt)
                       | y < 0 || y > 59 = error ("Wrong Primes in: " ++ pt)
                       | z < 0 || z > 59 = error ("Wrong Latters in: " ++ pt)
                       | otherwise = True 
                       where
                           pt = " " ++ show s ++ " " ++ show x ++ " " ++ show y++ " " ++ show z

{-Verify Latitude Sign.
* Input: A Latitude Tupla.
* Output: True if the Sign is Real. False otherwise.-}
verifyLat :: (Char, Int, Int, Float) -> Bool 
verifyLat (s, x, y, z)  
                      | verDetBody (s,x,y,z) && s == 'N' = True
                      | s == 'S' = True 
                      | otherwise = error ("Wrong Sign in: " ++ pt) 
                      where 
                          pt = " " ++ show s ++ " " ++ show x ++ " " ++ show y++ " " ++ show z

{-Verify Longitude Sign.
* Input: A Longitude Tupla.
* Output: True if the Sign is Real. False otherwise.-}
verifyLon :: (Char, Int, Int, Float) -> Bool 
verifyLon (s, x, y, z)  
                      | verDetBody (s,x,y,z) && s == 'E' = True
                      | s == 'W' = True 
                      | otherwise = error ("Wrong Sign in: " ++ pt) 
                      where 
                          pt = " " ++ show s ++ " " ++ show x ++ " " ++ show y++ " " ++ show z
     
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
getPoint st
           | length st < 31 || length st > 31 = error ("Invalid Argument: " ++ st)
           | otherwise = 
                         if verifyLat (getLatitude st) && verifyLon (getLongitude st) 
                         then merge (convertToDecimal (getLatitude st)) (convertToDecimal (getLongitude st))
                         else []