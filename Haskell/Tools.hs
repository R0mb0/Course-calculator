module Tools where

{-A function to round to 5 decimal places-}
round5dp :: Double -> Double
round5dp x = fromIntegral (round $ x * 1e5) / 1e5

{-A function to round to 2 decimal places-}
round2dp :: Double -> Double
round2dp x = fromIntegral (round $ x * 1e2) / 1e2