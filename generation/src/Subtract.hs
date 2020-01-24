{-# LANGUAGE QuasiQuotes #-}
module Subtract(
    safeSubtractOps
  , unsafeSubtractOps
  , safeSignedSubtractOps
  , unsafeSignedSubtractOps
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

safeSubtractOps :: File
safeSubtractOps = File {
    predicate = \ me others -> (me + 64) `elem` others,
    outputName = "safe_sub",
    isUnsigned = True,
    generator = declareSafeSubtractOperators,
    testCase = Just generateSafeTests
}

safeSignedSubtractOps :: File
safeSignedSubtractOps = File {
    predicate = \ me others -> (me + 64) `elem` others,
    outputName = "safe_ssub",
    isUnsigned = True,
    generator = declareSafeSignedSubtractOperators,
    testCase = Just generateSafeSignedTests
}

unsafeSubtractOps :: File
unsafeSubtractOps = File {
    predicate = \ _ _ -> True,
    outputName = "unsafe_sub",
    isUnsigned = True,
    generator = declareUnsafeSubtractOperators,
    testCase = Just generateUnsafeTests
}

unsafeSignedSubtractOps :: File
unsafeSignedSubtractOps = File {
    predicate = \ _ _ -> True,
    outputName = "unsafe_ssub",
    isUnsigned = True,
    generator = declareUnsafeSignedSubtractOperators,
    testCase = Just generateUnsafeSignedTests
}

declareSafeSubtractOperators :: Word -> [Word] -> SourceFile Span
declareSafeSubtractOperators bitsize _ =
  let sname = mkIdent ("U" ++ show bitsize)
      dname = mkIdent ("U" ++ show (bitsize + 64))
      fullRippleSubtract = makeRippleSubtracter True (bitsize `div` 64) "res"
      testFileLit = Lit [] (Str (testFile True bitsize) Cooked Unsuffixed mempty) mempty
  in [sourceFile|
        use core::ops::Sub;
        use crate::CryptoNum;
        #[cfg(test)]
        use crate::testing::{build_test_path,run_test};
        use crate::unsigned::{$$sname,$$dname};

        impl Sub for $$sname {
          type Output = $$dname;

          fn sub(self, rhs: $$sname) -> $$dname {
             &self - &rhs
          }
        }

        impl<'a> Sub<&'a $$sname> for $$sname {
          type Output = $$dname;

          fn sub(self, rhs: &$$sname) -> $$dname {
            &self - rhs
          }
        }

        impl<'a> Sub<$$sname> for &'a $$sname {
          type Output = $$dname;

          fn sub(self, rhs: $$sname) -> $$dname {
            self - &rhs
          }
        }

        impl<'a,'b> Sub<&'a $$sname> for &'b $$sname {
          type Output = $$dname;

          fn sub(self, rhs: &$$sname) -> $$dname {
            let mut res = $$dname::zero();

            $@{fullRippleSubtract}

            res
          }
        }

       #[cfg(test)]
       #[allow(non_snake_case)]
       #[test]
       fn KATs() {
         run_test(build_test_path("safe_sub", $$(testFileLit)), 3, |case| {
           let (neg0, xbytes) = case.get("x").unwrap();
           let (neg1, ybytes) = case.get("y").unwrap();
           let (neg2, zbytes) = case.get("z").unwrap();

           assert!(!neg0 && !neg1 && !neg2);
           let x = $$sname::from_bytes(&xbytes);
           let y = $$sname::from_bytes(&ybytes);
           let z = $$dname::from_bytes(&zbytes);

           assert_eq!(z, x - y);
        });
      }
      |]

declareSafeSignedSubtractOperators :: Word -> [Word] -> SourceFile Span
declareSafeSignedSubtractOperators bitsize _ =
  let sname = mkIdent ("I" ++ show bitsize)
      dname = mkIdent ("I" ++ show (bitsize + 64))
      testFileLit = Lit [] (Str (testFile True bitsize) Cooked Unsuffixed mempty) mempty
  in [sourceFile|
        use core::ops::Sub;
        use crate::CryptoNum;
        #[cfg(test)]
        use crate::testing::{build_test_path,run_test};
        use crate::signed::{$$sname,$$dname};

        impl Sub for $$sname {
          type Output = $$dname;

          fn sub(self, rhs: $$sname) -> $$dname {
             &self - &rhs
          }
        }

        impl<'a> Sub<&'a $$sname> for $$sname {
          type Output = $$dname;

          fn sub(self, rhs: &$$sname) -> $$dname {
            &self - rhs
          }
        }

        impl<'a> Sub<$$sname> for &'a $$sname {
          type Output = $$dname;

          fn sub(self, rhs: $$sname) -> $$dname {
            self - &rhs
          }
        }

        impl<'a,'b> Sub<&'a $$sname> for &'b $$sname {
          type Output = $$dname;

          fn sub(self, rhs: &$$sname) -> $$dname {
            panic!("sub")
          }
        }

       #[cfg(test)]
       #[allow(non_snake_case)]
       #[test]
       fn KATs() {
         run_test(build_test_path("safe_sub", $$(testFileLit)), 3, |case| {
           let (neg0, xbytes) = case.get("x").unwrap();
           let (neg1, ybytes) = case.get("y").unwrap();
           let (neg2, zbytes) = case.get("z").unwrap();

           assert!(!neg0 && !neg1 && !neg2);
           let x = $$sname::from_bytes(&xbytes);
           let y = $$sname::from_bytes(&ybytes);
           let z = $$dname::from_bytes(&zbytes);

           assert_eq!(z, x - y);
        });
      }
      |]

declareUnsafeSubtractOperators :: Word -> [Word] -> SourceFile Span
declareUnsafeSubtractOperators bitsize _ =
  let sname = mkIdent ("U" ++ show bitsize)
      fullRippleSubtract = makeRippleSubtracter False (bitsize `div` 64) "self"
      testFileLit = Lit [] (Str (testFile True bitsize) Cooked Unsuffixed mempty) mempty
  in [sourceFile|
        use core::ops::SubAssign;
        #[cfg(test)]
        use crate::CryptoNum;
        #[cfg(test)]
        use crate::testing::{build_test_path,run_test};
        use crate::unsigned::$$sname;

        impl SubAssign for $$sname {
          fn sub_assign(&mut self, rhs: Self) {
            self.sub_assign(&rhs);
          }
        }

        impl<'a> SubAssign<&'a $$sname> for $$sname {
          fn sub_assign(&mut self, rhs: &Self) {
            $@{fullRippleSubtract}
          }
        }

       #[cfg(test)]
       #[allow(non_snake_case)]
       #[test]
       fn KATs() {
         run_test(build_test_path("unsafe_sub", $$(testFileLit)), 3, |case| {
           let (neg0, xbytes) = case.get("x").unwrap();
           let (neg1, ybytes) = case.get("y").unwrap();
           let (neg2, zbytes) = case.get("z").unwrap();

           assert!(!neg0 && !neg1 && !neg2);
           let mut x = $$sname::from_bytes(&xbytes);
           let y     = $$sname::from_bytes(&ybytes);
           let z     = $$sname::from_bytes(&zbytes);

           x -= &y;
           assert_eq!(z, x);
        });
      }
      |]

declareUnsafeSignedSubtractOperators :: Word -> [Word] -> SourceFile Span
declareUnsafeSignedSubtractOperators bitsize _ =
  let sname = mkIdent ("I" ++ show bitsize)
      testFileLit = Lit [] (Str (testFile True bitsize) Cooked Unsuffixed mempty) mempty
  in [sourceFile|
        use core::ops::SubAssign;
        #[cfg(test)]
        use crate::CryptoNum;
        #[cfg(test)]
        use crate::testing::{build_test_path,run_test};
        use crate::signed::$$sname;

        impl SubAssign for $$sname {
          fn sub_assign(&mut self, rhs: Self) {
            self.sub_assign(&rhs);
          }
        }

        impl<'a> SubAssign<&'a $$sname> for $$sname {
          fn sub_assign(&mut self, rhs: &Self) {
            panic!("sub_assign")
          }
        }

       #[cfg(test)]
       #[allow(non_snake_case)]
       #[test]
       fn KATs() {
         run_test(build_test_path("unsafe_ssub", $$(testFileLit)), 3, |case| {
           let (neg0, xbytes) = case.get("x").unwrap();
           let (neg1, ybytes) = case.get("y").unwrap();
           let (neg2, zbytes) = case.get("z").unwrap();

           let mut x = $$sname::from_bytes(&xbytes);
           let mut y = $$sname::from_bytes(&ybytes);
           let mut z = $$sname::from_bytes(&zbytes);
           if *neg0 { x = -x; }
           if *neg1 { y = -y; }
           if *neg2 { z = -z; }

           x -= &y;
           assert_eq!(z, x);
        });
      }
      |]

makeRippleSubtracter :: Bool -> Word -> String -> [Stmt Span]
makeRippleSubtracter useLastCarry inElems resName =
  concatMap (generateRipples useLastCarry (inElems - 1)) [0..inElems-1] ++
  concatMap (generateSetters useLastCarry inElems resName) [0..inElems]

generateRipples :: Bool -> Word -> Word -> [Stmt Span]
generateRipples useLastCarry lastI i =
  let sumi = mkIdent ("sum" ++ show i)
      inCarry = mkIdent ("carry" ++ show (i - 1))
      outCarry = mkIdent ("carry" ++ show i)
      res = mkIdent ("res" ++ show i)
      liti = toLit i
      left = mkIdent ("left" ++ show i)
      right = mkIdent ("right" ++ show i)
  in [
    [stmt|let $$left = self.value[$$(liti)] as u128; |]
  , [stmt|let $$right = !rhs.value[$$(liti)] as u128; |]
  , if i == 0
      then [stmt| let $$sumi = $$left + $$right + 1; |]
      else [stmt| let $$sumi = $$left + $$right + $$inCarry; |]
  , [stmt|let $$res = $$sumi as u64; |]
  ] ++
  if not useLastCarry && (i == lastI)
    then []
    else [[stmt|let $$outCarry = $$sumi >> 64; |]]

generateSetters :: Bool -> Word -> String -> Word -> [Stmt Span]
generateSetters useLastCarry maxI resName i
  | not useLastCarry && (maxI == i) = []
  | maxI == i =
      let res = mkIdent ("carry" ++ show (i - 1))
          liti = toLit i
      in [[stmt| $$target.value[$$(liti)] = (0xFFFFFFFFFFFFFFFFu128 + $$res) as u64; |]]
  | otherwise =
      let res = mkIdent ("res" ++ show i)
          liti = toLit i
      in [[stmt| $$target.value[$$(liti)] = $$res; |]]
 where
  target = mkIdent resName

generateSafeTests :: RandomGen g => Word -> g -> [Map String String]
generateSafeTests size g = go g numTestCases
 where
  go _  0 = []
  go g0 i =
    let (x, g1)       = generateNum g0 size
        (y, g2)       = generateNum g1 size
        r | x < y     = (2 ^ (size + 64)) + (x - y)
          | otherwise = x - y
        tcase         = Map.fromList [("x", showX x),
                                      ("y", showX y),
                                      ("z", showX r)]
    in tcase : go g2 (i - 1)

generateSafeSignedTests :: RandomGen g => Word -> g -> [Map String String]
generateSafeSignedTests size g = go g numTestCases
 where
  go _  0 = []
  go g0 i =
    let (x, g1)       = generateSignedNum g0 size
        (y, g2)       = generateSignedNum g1 size
        r | x < y     = (2 ^ (size + 64)) + (x - y)
          | otherwise = x - y
        tcase         = Map.fromList [("x", showX x),
                                      ("y", showX y),
                                      ("z", showX r)]
    in tcase : go g2 (i - 1)

generateUnsafeTests :: RandomGen g => Word -> g -> [Map String String]
generateUnsafeTests size g = go g numTestCases
 where
  go _  0 = []
  go g0 i =
    let (x, g1) = generateNum g0 size
        (y, g2) = generateNum g1 size
        z       = (x - y) .&. ((2 ^ size) - 1)
        tcase   = Map.fromList [("x", showX x), ("y", showX y),
                                ("z", showX z)]
    in tcase : go g2 (i - 1)

generateUnsafeSignedTests :: RandomGen g => Word -> g -> [Map String String]
generateUnsafeSignedTests size g = go g numTestCases
 where
  go _  0 = []
  go g0 i =
    let (x, g1) = generateSignedNum g0 size
        (y, g2) = generateSignedNum g1 size
        z       = (x - y) .&. ((2 ^ size) - 1)
        tcase   = Map.fromList [("x", showX x), ("y", showX y),
                                ("z", showX z)]
    in tcase : go g2 (i - 1)
