module Tools where

{-***Defining Costants***-}
eartRadius :: Double 
eartRadius = 6372.795477598

{-***Defining Functions to calculate the course properties***-}

{-Calculate the distance between two detections
 -Formula implemented:
 -distance (A,B) = R * arccos(sin(latA) * sin(latB) + cos(latA) * cos(latB) * cos(lonA-lonB))-}
distance :: [Double] -> [Double] -> Double 
distance detA detB = eartRadius * acos(sin(detA !! 0 * pi / 180) * sin(detB !! 0 * pi / 180) + cos(detA !! 0 * pi / 180) * cos(detB !! 0 * pi / 180) * cos((detA !! 1 - detB !! 1) * pi / 180)) 

{-Calculate the directions between two detections
 - Formula Implemented: 
 - Δφ = ln( tan( latB / 2 + π / 4 ) / tan( latA / 2 + π / 4) )
 - Δlon = abs( lonA - lonB ) 
 - direction :  θ = atan2( Δlon ,  Δφ )-}

{-Defining the first part of formula-}
phi :: [Double] -> [Double] -> Double 
phi detA detB = log(tan((detB !! 0 * pi / 180) / 2 + pi / 4) / tan((detA !! 0 * pi / 180) / 2 + pi / 4))

{-Defining the second part of formula-}
lon :: [Double] -> [Double] -> Double
lon detA detB = abs(detA !! 1 - detB !! 1)

{-Defining a lon nomralizer-}
verLon :: Double -> Double 
verLon val
          | (val * pi / 180) > 180 = fromInteger(rem (round val :: Integer) 180)   --val 180
          | otherwise = val 

{-Defining directions calculator-}
direction :: [Double] -> [Double] -> Double 
direction det1 det2 = atan2(verLon(lon det1 det2) * pi / 180) (abs (phi det1 det2)) / pi * 180

{-Defining a funciton that calculate the contrary direction-}
invDirection :: [Double] -> [Double] -> Double
invDirection det1 det2 = direction det1 det2 + 180