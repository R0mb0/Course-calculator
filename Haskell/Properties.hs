module Properties where

{-*** Defining functions to calculate the course properties. ***-}

{-Calculate the distance between two detections.
* Input: Two lists of double containing the two detections in decimal format.
* Output: A dobule containing the distance between the two detections in Km.-}
distance :: [Double] -> [Double] -> Double 
distance [] [_] = error "The first argument is null"
distance [_] [] = error "The second argument is null"
distance detA detB
                | length detA > 2 = error "The first argument has too many elements"
                | length detB > 2 = error "The second argument has too many elements"
                | otherwise = 6372.795477598 * acos (sin latA * sin latB + cos latA * cos latB * cos ((lonA - lonB) * pi / 180))
                where
                    latA = head detA * pi / 180
                    latB = head detB * pi / 180
                    lonA = detA !! 1
                    lonB = detB !! 1

{-Calculate the directions angle between two detections.-}

{-** Directions. **-}

{-Calculate the delta phi between two coordinates.
* Input: Two lists of double containing the two detections in decimal format.
* Output: A dobule containing the delta phi between the two detections.-}
phi :: [Double] -> [Double] -> Double 
phi [] [_] = error "The first argument is null"
phi [_] [] = error "The second argument is null"
phi detA detB 
             | length detA > 2 = error "The first argument has too many elements"
             | length detB > 2 = error "The second argument has too many elements"
             | head detA == head detB = pi / 180 * 0.000000001
             | otherwise = log (tan (latB / 2 + pi / 4) / tan (latA / 2 + pi / 4))
             where
                 latA = head detA * pi / 180
                 latB = head detB * pi / 180

{-Verify if delta longitude must be normalized.
* Input: A double containing the delta longitude value.
* Output: A double containing the correct delta longitude value.-}
verLon :: Double -> Double 
verLon val
          | (val * pi / 180) > 180 = fromInteger (rem (round val :: Integer) 180)   
          | otherwise = val

{-Calculate the delta longitude between two coordinates.
* Input: Two lists of double containing the two detections in decimal format.
* Output: A dobule containing the delta longitude between the two detections.-}
lon :: [Double] -> [Double] -> Double
lon [] [_] = error "The first argument is null"
lon [_] [] = error "The second argument is null"
lon detA detB
             | length detA > 2 = error "The first argument has too many elements"
             | length detB > 2 = error "The second argument has too many elements"
             | detA !! 1 == detB !! 1 = pi / 180 * 0.000000001
             | otherwise = verLon (abs (detA !! 1 - detB !! 1)) 

{-Calculate the direction angle between two coordinates.
* Input: Two lists of double containing the two detections in decimal format.
* Output: A dobule containing the direction angle between the two detections.-}
direction :: [Double] -> [Double] -> Double 
direction [] [_] = error "The first argument is null"
direction [_] [] = error "The second argument is null"
direction detA detB 
                   | length detA > 2 = error "The first argument has too many elements"
                   | length detB > 2 = error "The second argument has too many elements"
                   | otherwise = atan2 (lon detA detB * pi / 180) (abs (phi detA detB)) / pi * 180

{-Calculate the inverse direction angle between two coordinates.
* Input: Two lists of double containing the two detections in decimal format.
* Output: A dobule containing the inverse direction angle between the two detections.-}
invDirection :: [Double] -> [Double] -> Double
invDirection [] [_] = error "The first argument is null"
invDirection [_] [] = error "The second argument is null"
invDirection detA detB 
                      | length detA > 2 = error "The first argument has too many elements"
                      | length detB > 2 = error "The second argument has too many elements"
                      | otherwise = direction detA detB + 180