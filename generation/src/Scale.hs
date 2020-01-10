{-# LANGUAGE QuasiQuotes #-}
module Scale(
    safeScaleOps
  , unsafeScaleOps
  )
 where

import Data.Bits((.&.))
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import File
import Gen(toLit)
import Generators
import Language.Rust.Data.Ident
import Language.Rust.Data.Position
import Language.Rust.Quote
import Language.Rust.Syntax
import System.Random(RandomGen)

numTestCases :: Int
numTestCases = 3000

safeScaleOps :: File
safeScaleOps = File {
    predicate = \ me others -> (me + 64) `elem` others,
    outputName = "safe_scale",
    isUnsigned = True,
    generator = declareSafeScaleOperators,
    testCase = Just generateSafeTests
}

unsafeScaleOps :: File
unsafeScaleOps = File {
    predicate = \ _ _ -> True,
    outputName = "unsafe_scale",
    isUnsigned = True,
    generator = declareUnsafeScaleOperators,
    testCase = Just generateUnsafeTests
}

declareSafeScaleOperators :: Word -> [Word] -> SourceFile Span
declareSafeScaleOperators bitsize _ =
  let sname = mkIdent ("U" ++ show bitsize)
      dname = mkIdent ("U" ++ show (bitsize + 64))
      fullRippleScale = generateScaletiplier True (bitsize `div` 64) "rhs" "res"
      testFileLit = Lit [] (Str (testFile bitsize) Cooked Unsuffixed mempty) mempty
  in [sourceFile|
        use core::ops::Mul;
        use crate::CryptoNum;
        #[cfg(test)]
        use crate::testing::{build_test_path,run_test};
        use crate::unsigned::{$$sname,$$dname};

        impl Mul<u8> for $$sname {
          type Output = $$dname;

          fn mul(self, rhs: u8) -> $$dname {
             &self * (rhs as u64)
          }
        }

        impl<'a> Mul<u8> for &'a $$sname {
            type Output = $$dname;

            fn mul(self, rhs: u8) -> $$dname {
                self * (rhs as u64)
            }
        }

        impl Mul<u16> for $$sname {
          type Output = $$dname;

          fn mul(self, rhs: u16) -> $$dname {
             &self * (rhs as u64)
          }
        }

        impl<'a> Mul<u16> for &'a $$sname {
            type Output = $$dname;

            fn mul(self, rhs: u16) -> $$dname {
                self * (rhs as u64)
            }
        }

        impl Mul<u32> for $$sname {
          type Output = $$dname;

          fn mul(self, rhs: u32) -> $$dname {
             &self * (rhs as u64)
          }
        }

        impl<'a> Mul<u32> for &'a $$sname {
            type Output = $$dname;

            fn mul(self, rhs: u32) -> $$dname {
                self * (rhs as u64)
            }
        }

        impl Mul<usize> for $$sname {
          type Output = $$dname;

          fn mul(self, rhs: usize) -> $$dname {
             &self * (rhs as u64)
          }
        }

        impl<'a> Mul<usize> for &'a $$sname {
            type Output = $$dname;

            fn mul(self, rhs: usize) -> $$dname {
                self * (rhs as u64)
            }
        }

        impl Mul<u64> for $$sname {
          type Output = $$dname;

          fn mul(self, rhs: u64) -> $$dname {
             &self * (rhs as u64)
          }
        }

        impl<'a> Mul<u64> for &'a $$sname {
            type Output = $$dname;

            fn mul(self, rhs: u64) -> $$dname {
                let mut res = $$dname::zero();

                $@{fullRippleScale}

                res
            }
        }

       #[cfg(test)]
       #[allow(non_snake_case)]
       #[test]
       fn KATs() {
         run_test(build_test_path("safe_scale", $$(testFileLit)), 3, |case| {
           let (neg0, xbytes) = case.get("x").unwrap();
           let (neg1, ybytes) = case.get("y").unwrap();
           let (neg2, zbytes) = case.get("z").unwrap();

           assert!(!neg0 && !neg1 && !neg2);
           let x = $$sname::from_bytes(&xbytes);
           let y = $$sname::from_bytes(&ybytes);
           let z = $$dname::from_bytes(&zbytes);

           assert_eq!(z, x * y.value[0]);
        });
      }
      |]

declareUnsafeScaleOperators :: Word -> [Word] -> SourceFile Span
declareUnsafeScaleOperators bitsize _ =
  let sname = mkIdent ("U" ++ show bitsize)
      halfRippleScale = generateScaletiplier False (bitsize `div` 64) "rhs" "self"
      testFileLit = Lit [] (Str (testFile bitsize) Cooked Unsuffixed mempty) mempty
  in [sourceFile|
        use core::ops::MulAssign;
        #[cfg(test)]
        use crate::CryptoNum;
        #[cfg(test)]
        use crate::testing::{build_test_path,run_test};
        use crate::unsigned::$$sname;

        impl MulAssign<u8> for $$sname {
          fn mul_assign(&mut self, rhs: u8) {
            self.mul_assign(rhs as u64);
          }
        }

        impl MulAssign<u16> for $$sname {
          fn mul_assign(&mut self, rhs: u16) {
            self.mul_assign(rhs as u64);
          }
        }

        impl MulAssign<u32> for $$sname {
          fn mul_assign(&mut self, rhs: u32) {
            self.mul_assign(rhs as u64);
          }
        }

        impl MulAssign<usize> for $$sname {
          fn mul_assign(&mut self, rhs: usize) {
            self.mul_assign(rhs as u64);
          }
        }

        impl MulAssign<u64> for $$sname {
          fn mul_assign(&mut self, rhs: u64) {
            $@{halfRippleScale}
          }
        }

       #[cfg(test)]
       #[allow(non_snake_case)]
       #[test]
       fn KATs() {
         run_test(build_test_path("unsafe_scale", $$(testFileLit)), 3, |case| {
           let (neg0, xbytes) = case.get("x").unwrap();
           let (neg1, ybytes) = case.get("y").unwrap();
           let (neg2, zbytes) = case.get("z").unwrap();

           assert!(!neg0 && !neg1 && !neg2);
           let mut x = $$sname::from_bytes(&xbytes);
           let y = $$sname::from_bytes(&ybytes);
           let z = $$sname::from_bytes(&zbytes);

           x *= y.value[0];
           assert_eq!(z, x);
        });
      }
      |]

-- -----------------------------------------------------------------------------

generateScaletiplier :: Bool -> Word -> String -> String -> [Stmt Span]
generateScaletiplier full size input output = loaders ++ [bigy] ++ ripples ++
                                              carryCatch ++ stores
 where
  outSize | full      = size + 1
          | otherwise = size
  loaders = map load [0..size-1]
  bigy = let invar = mkIdent input
         in [stmt| let y = $$invar as u128; |]
  ripples = map scale [0..size-1]
  carryCatch | not full  = []
             | otherwise = let outvar = mkIdent ("scaled" ++ show size)
                               lastv = mkIdent ("scaled" ++ show (size - 1))
                           in [[stmt| let $$outvar = ($$lastv >> 64) as u64; |]]
  stores = map store [0..outSize-1]
  --
  load i =
      let var = mkIdent ("x" ++ show i)
          liti = toLit i
      in [stmt| let $$var = self.value[$$(liti)]; |]
  scale i =
      let out  = mkIdent ("scaled" ++ show i)
          x    = mkIdent ("x" ++ show i)
          y    = mkIdent "y"
          --
          prevName = mkIdent ("scaled" ++ show (i - 1))
          prev | i == 0    = toLit 0
               | otherwise = [expr| $$prevName >> 64 |]
      in [stmt| let $$out = ($$x as u128) * $$y + $$(prev); |]
  store i =
      let var = mkIdent ("scaled" ++ show i)
          out = mkIdent output
          liti = toLit i
      in [stmt| $$out.value[$$(liti)] = $$var as u64; |]

-- -----------------------------------------------------------------------------

generateSafeTests :: RandomGen g => Word -> g -> [Map String String]
generateSafeTests size g = go g numTestCases
 where
  go _  0 = []
  go g0 i =
    let (x, g1) = generateNum g0 size
        (y, g2) = generateNum g1 64
        tcase   = Map.fromList [("x", showX x), ("y", showX y),
                                ("z", showX (x * y))]
    in tcase : go g2 (i - 1)

generateUnsafeTests :: RandomGen g => Word -> g -> [Map String String]
generateUnsafeTests size g = go g numTestCases
 where
  go _  0 = []
  go g0 i =
    let (x, g1) = generateNum g0 size
        (y, g2) = generateNum g1 64
        z       = (x * y) .&. ((2 ^ size) - 1)
        tcase   = Map.fromList [("x", showX x), ("y", showX y),
                                ("z", showX z)]
    in tcase : go g2 (i - 1)

