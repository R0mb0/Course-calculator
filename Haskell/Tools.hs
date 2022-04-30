module Tools where

{-A function to round to 5 decimal places-}
round5dp :: Double -> Double
round5dp x = fromIntegral (round $ x * 1e5) / 1e5

{-A function to round to 2 decimal places-}
round2dp :: Double -> Double
round2dp x = fromIntegral (round $ x * 1e2) / 1e2

{-A function to verify if the string had the correct length-}
verStr :: String -> IO ( ) 
verStr [] = error "Null Argument"
verStr str
          | length str == 31 = putStr ""
          | otherwise = error "Bad Detection"