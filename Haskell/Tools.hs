module Tools where

{-Function to round a number to 3 decimals after the comma.
* Input: A dobule number.
* Output: A double number containing the input number rounded.-}
round3dp :: Double -> Double
round3dp x = fromIntegral (round $ x * 1e3) / 1e3

{-Function to round a number to 2 decimals after the comma.
* Input: A dobule number.
* Output: A double number containing the input number rounded.-}
round2dp :: Double -> Double
round2dp x = fromIntegral (round $ x * 1e2) / 1e2