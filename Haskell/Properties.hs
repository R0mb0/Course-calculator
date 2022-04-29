module Properties where

{-***Defining Costants***-}
eartRadius :: Double 
eartRadius = 6372.795477598

{-***Defining Functions to calculate the course properties***-}

{-Calculate the distance between two detections
 -Formula implemented:
 -distance (A,B) = R * arccos(sin(latA) * sin(latB) + cos(latA) * cos(latB) * cos(lonA-lonB))-}
distance :: [Double] -> [Double] -> Double 
distance [] [_] = error "The First Argument Is Null"
distance [_] [] = error "The Second Argument Is Null"
distance detA detB
                | length detA > 2 = error "The First Argument Has Too Much Elements"
                | length detB > 2 = error "The Second Argument Has Too Much Elements"
                | otherwise = eartRadius * acos (sin latA * sin latB + cos latA * cos latB * cos((lonA - lonB) * pi / 180))
                where
                    latA = head detA * pi / 180
                    latB = head detB * pi / 180
                    lonA = detA !! 1
                    lonB = detB !! 1

{-Calculate the directions between two detections
 - Formula Implemented: 
 - Δφ = ln( tan( latB / 2 + π / 4 ) / tan( latA / 2 + π / 4) )
 - Δlon = abs( lonA - lonB ) 
 - direction :  θ = atan2( Δlon ,  Δφ )-}

{-**Directions**-}

{-Defining the first part of formula-}
phi :: [Double] -> [Double] -> Double 
phi [] [_] = error "The First Argument Is Null"
phi [_] [] = error "The Second Argument Is Null"
phi detA detB 
             | length detA > 2 = error "The First Argument Has Too Much Elements"
             | length detB > 2 = error "The Second Argument Has Too Much Elements"
             |head detA == head detB = pi / 180 * 0.000000001
             |otherwise = log(tan(latB / 2 + pi / 4) / tan(latA / 2 + pi / 4))
             where
                 latA = head detA * pi / 180
                 latB = head detB * pi / 180

{-Defining a lon nomralizer-}
verLon :: Double -> Double 
verLon val
          | (val * pi / 180) > 180 = fromInteger(rem (round val :: Integer) 180)   
          | otherwise = val

{-Defining the second part of formula-}
lon :: [Double] -> [Double] -> Double
lon [] [_] = error "The First Argument Is Null"
lon [_] [] = error "The Second Argument Is Null"
lon detA detB
             | length detA > 2 = error "The First Argument Has Too Much Elements"
             | length detB > 2 = error "The Second Argument Has Too Much Elements"
             | detA !! 1 == detB !! 1 = pi / 180 * 0.000000001
             | otherwise = verLon(abs(detA !! 1 - detB !! 1)) 

{-Defining directions calculator-}
direction :: [Double] -> [Double] -> Double 
direction [] [_] = error "The First Argument Is Null"
direction [_] [] = error "The Second Argument Is Null"
direction detA detB 
                   | length detA > 2 = error "The First Argument Has Too Much Elements"
                   | length detB > 2 = error "The Second Argument Has Too Much Elements"
                   | otherwise = atan2(lon detA detB * pi / 180) (abs (phi detA detB)) / pi * 180

{-Defining a funciton that calculate the contrary direction-}
invDirection :: [Double] -> [Double] -> Double
invDirection [] [_] = error "The First Argument Is Null"
invDirection [_] [] = error "The Second Argument Is Null"
invDirection detA detB 
                      | length detA > 2 = error "The First Argument Has Too Much Elements"
                      | length detB > 2 = error "The Second Argument Has Too Much Elements"
                      | otherwise = direction detA detB + 180