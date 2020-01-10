import Data.Bits hiding (bit)
import Debug.Trace
import GHC.Integer.GMP.Internals
import qualified Karatsuba
import Numeric
import Test.QuickCheck

modular_exponentiation :: Integer -> Integer -> Integer -> Integer
modular_exponentiation x y m = m_e_loop x y 1
 where
  m_e_loop _ 0 result = result
  m_e_loop b e result = m_e_loop b' e' result'
   where
    b'      = (b * b) `mod` m
    e'      = e `shiftR` 1
    result' = if testBit e 0 then (result * b) `mod` m else result

prop_modExpSane :: Integer -> Integer -> Integer -> Property
prop_modExpSane b e m = (m' > 1) ==> modular_exponentiation b' e' m' == powModInteger b' e' m'
 where
  b' = abs b
  e' = abs e
  m' = abs m

modexpLR :: Int -> Integer -> Integer -> Integer -> Integer
modexpLR bitsize b e m = go (bitsize - 1) 1
 where
   go bit r0
     | bit < 0       = r0
     | testBit e bit = trace ("1: r = " ++ showHex r2 "") $ go (bit - 1) r2
     | otherwise     = trace ("0: r = " ++ showHex r1 "") $ go (bit - 1) r1
    where
     r1 = (r0 * r0) `mod` m
     r2 = (r1 * b)  `mod` m

prop_modExpLR192 :: Integer -> Integer -> Integer -> Property
prop_modExpLR192 b e m = (m' > 1) ==> modexpLR 192 b' e' m' == powModInteger b' e' m'
 where
  b' = abs b `mod` (2 ^ (192 :: Integer))
  e' = abs e `mod` (2 ^ (192 :: Integer))
  m' = abs m `mod` (2 ^ (192 :: Integer))

main :: IO ()
main =
  do Karatsuba.runChecks 
     Karatsuba.runQuickCheck "Modular exponentiation sanity check" prop_modExpSane
     Karatsuba.runQuickCheck "ModExp LR 192 works" prop_modExpLR192
