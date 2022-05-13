module Tools where

{-Function to Round a Number to 5 Decimals After the Comma.
* Input: A Dobule Number.
* Output: A Double Number Containing the Input Number Rounded.-}
round5dp :: Double -> Double
round5dp x = fromIntegral (round $ x * 1e5) / 1e5

{-Function to Round a Number to 2 Decimals After the Comma.
* Input: A Dobule Number.
* Output: A Double Number Containing the Input Number Rounded.-}
round2dp :: Double -> Double
round2dp x = fromIntegral (round $ x * 1e2) / 1e2