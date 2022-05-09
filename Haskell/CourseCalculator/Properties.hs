module Properties where

{-*** Defining Costants. ***-}
eartRadius :: Double 
eartRadius = 6372.795477598

{-*** Defining Functions to Calculate the Course Properties. ***-}

{-Calculate the Distance Between Two Detections.
-> Input: Two Lists of Double Containing the two Detections in Decimal format.
-> Output: A Dobule Containing the Distance Between the Two Detections in Km.-}
distance :: [Double] -> [Double] -> Double 
distance [] [_] = error "The First Argument Is Null"
distance [_] [] = error "The Second Argument Is Null"
distance detA detB
                | length detA > 2 = error "The First Argument Has Too Much Elements"
                | length detB > 2 = error "The Second Argument Has Too Much Elements"
                | otherwise = eartRadius * acos (sin latA * sin latB + cos latA * cos latB * cos ((lonA - lonB) * pi / 180))
                where
                    latA = head detA * pi / 180
                    latB = head detB * pi / 180
                    lonA = detA !! 1
                    lonB = detB !! 1

{-Calculate the Directions Angle Between Two Detections.-}

{-** Directions. **-}

{-Calculate the Delta Phi Between two Coordinates.
-> Input: Two Lists of Double Containing the two Detections in Decimal format.
-> Output: A Dobule Containing the Delta Phi Between the Two Detections.-}
phi :: [Double] -> [Double] -> Double 
phi [] [_] = error "The First Argument Is Null"
phi [_] [] = error "The Second Argument Is Null"
phi detA detB 
             | length detA > 2 = error "The First Argument Has Too Much Elements"
             | length detB > 2 = error "The Second Argument Has Too Much Elements"
             |head detA == head detB = pi / 180 * 0.000000001
             |otherwise = log (tan (latB / 2 + pi / 4) / tan (latA / 2 + pi / 4))
             where
                 latA = head detA * pi / 180
                 latB = head detB * pi / 180

{-Verify if Delta Longitude must be Nomralized.
-> Input: A Double Containing the Delta Longitude value.
-> Output: A Double Containing the Correct Delta Longitude value.-}
verLon :: Double -> Double 
verLon val
          | (val * pi / 180) > 180 = fromInteger (rem (round val :: Integer) 180)   
          | otherwise = val

{-Calculate the Delta Longitude Between two Coordinates.
-> Input: Two Lists of Double Containing the two Detections in Decimal format.
-> Output: A Dobule Containing the Delta Longitude Between the Two Detections.-}
lon :: [Double] -> [Double] -> Double
lon [] [_] = error "The First Argument Is Null"
lon [_] [] = error "The Second Argument Is Null"
lon detA detB
             | length detA > 2 = error "The First Argument Has Too Much Elements"
             | length detB > 2 = error "The Second Argument Has Too Much Elements"
             | detA !! 1 == detB !! 1 = pi / 180 * 0.000000001
             | otherwise = verLon (abs (detA !! 1 - detB !! 1)) 

{-Calculate the Direction Angle Between two Coordinates.
-> Input: Two Lists of Double Containing the two Detections in Decimal format.
-> Output: A Dobule Containing the Direction Angle Between the Two Detections.-}
direction :: [Double] -> [Double] -> Double 
direction [] [_] = error "The First Argument Is Null"
direction [_] [] = error "The Second Argument Is Null"
direction detA detB 
                   | length detA > 2 = error "The First Argument Has Too Much Elements"
                   | length detB > 2 = error "The Second Argument Has Too Much Elements"
                   | otherwise = atan2 (lon detA detB * pi / 180) (abs (phi detA detB)) / pi * 180

{-Calculate the Inverse Direction Angle Between two Coordinates.
-> Input: Two Lists of Double Containing the two Detections in Decimal format.
-> Output: A Dobule Containing the Inverse Direction Angle Between the Two Detections.-}
invDirection :: [Double] -> [Double] -> Double
invDirection [] [_] = error "The First Argument Is Null"
invDirection [_] [] = error "The Second Argument Is Null"
invDirection detA detB 
                      | length detA > 2 = error "The First Argument Has Too Much Elements"
                      | length detB > 2 = error "The Second Argument Has Too Much Elements"
                      | otherwise = direction detA detB + 180