module Tools where

{-A function to round to 6 decimal places-}
round6dp :: Double -> Double
round6dp x = fromIntegral (round $ x * 1e6) / 1e6

{-A function to verify if the string had the correct length-}
verStr :: String -> IO ( ) 
verStr str
          | length str == 31 = putStr ""
          | otherwise = error "Bad Detection"