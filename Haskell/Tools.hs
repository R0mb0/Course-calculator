module Tools where

{-Function to Round a Number to 3 Decimals After the Comma.
* Input: A Dobule Number.
* Output: A Double Number Containing the Input Number Rounded.-}
round3dp :: Double -> Double
round3dp x = fromIntegral (round $ x * 1e3) / 1e3

{-Function to Round a Number to 2 Decimals After the Comma.
* Input: A Dobule Number.
* Output: A Double Number Containing the Input Number Rounded.-}
round2dp :: Double -> Double
round2dp x = fromIntegral (round $ x * 1e2) / 1e2