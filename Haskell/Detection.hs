module Detection where
import GHC.Float
import GHC.Num

{-****Defining the Functions to work with detection****-}
            
{- Split the longitude string part-}
split :: String -> String
split = drop 17 

{-Get the latitude tupla from the string-}
getLatitude :: String -> (Char, Int, Int, Float)
getLatitude [] = error "Null Argument"
getLatitude st
              | length st < 31 || length st > 31 = error "Invalid Argument"
              | otherwise = (head st, read(take 2 (drop 2 st)) :: Int, read(drop 5 (take 7 st)) :: Int, read(drop 8 (take 14 st)) :: Float)

{-Get the longitude tupla from the string-}
getLongitude :: String -> (Char, Int, Int, Float)
getLongitude [] = error "Null Argument"
getLongitude st
               | length st < 31 || length st > 31 = error "Invalid Argument"
               | otherwise = (head (split st), read(take 2 (drop 2 (split st))) :: Int, read(drop 5 (take 7 (split st))) :: Int, read(drop 8 (take 14 (split st))) :: Float)

{-**Verify if the latitude & the longitude are real**-}

{-Verify the coordinates body-}
verDetBody :: (Char, Int, Int, Float) -> Bool
verDetBody (s, x, y, z)
                       | x < 0 || x > 89 = error ("Wrong Degrees in: " ++ pt)
                       | y < 0 || y > 59 = error ("Wrong Prime in: " ++ pt)
                       | z < 0 || z > 59 = error ("Wrong Latter in: " ++ pt)
                       | otherwise = True 
                       where
                           pt = " " ++ show s ++ " " ++ show x ++ " " ++ show y++ " " ++ show z

{-Verify Latitude-}
verifyLat :: (Char, Int, Int, Float) -> Bool 
verifyLat (s, x, y, z)  
                      | verDetBody (s,x,y,z) && s == 'N' = True
                      | s == 'S' = True 
                      | otherwise = error ("Wrong Sign in: " ++ pt) 
                      where 
                          pt = " " ++ show s ++ " " ++ show x ++ " " ++ show y++ " " ++ show z

{-Verify Longitude-}
verifyLon :: (Char, Int, Int, Float) -> Bool 
verifyLon (s, x, y, z)  
                      | verDetBody (s,x,y,z) && s == 'E' = True
                      | s == 'W' = True 
                      | otherwise = error ("Wrong Sign in: " ++ pt) 
                      where 
                          pt = " " ++ show s ++ " " ++ show x ++ " " ++ show y++ " " ++ show z
     
{-Convert the coordinate in decimal format-}
convertToDecimal :: (Char, Int, Int , Float ) -> Double  
convertToDecimal (s, x, y, z) 
                             | s == 'S' || s == 'W' = float2Double (((((z / 60) + fromIntegral y) / 60) + fromIntegral x) * (-1))
                             | otherwise = float2Double ((((z / 60) + fromIntegral y) / 60) + fromIntegral x)

{-Create a merger in way to have the latitude & the longitude toghether-}
merge :: a -> a -> [a]
merge lat long = [lat, long]

{-Functions to get from the input string the decimal point-}
getPoint :: String -> [Double]
getPoint [] = error "Null Argument"
getPoint st
           | length st < 31 || length st > 31 = error "Invalid Argument"
           | otherwise = 
                         if verifyLat (getLatitude st) && verifyLon (getLongitude st) 
                         then merge (convertToDecimal(getLatitude st)) (convertToDecimal(getLongitude st))
                         else []
