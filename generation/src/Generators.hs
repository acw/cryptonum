module Generators
 where

import Numeric(showHex)
import System.Random(RandomGen,random)

generateNum :: RandomGen g => g -> Word -> (Integer, g)
generateNum g size =
  let (x, g') = random g
      x' = x `mod` (2 ^ size)
  in (x', g')

modulate :: (Integral a, Integral b) => a -> b -> Integer
modulate x size = x' `mod` (2 ^ size')
 where
  x', size' :: Integer
  size' = fromIntegral size
  x' = fromIntegral x

modulate' :: (Num a, Integral a, Integral b) => a -> b -> Integer
modulate' x size = signum x' * ((abs x') `mod` (2 ^ size'))
 where
  x', size' :: Integer
  size' = fromIntegral size
  x' = fromIntegral x

showX :: Integer -> String
showX x | x < 0     = "-" ++ showX (abs x)
        | otherwise = showHex x ""

showB :: Bool -> String
showB False = "0"
showB True  = "1"
