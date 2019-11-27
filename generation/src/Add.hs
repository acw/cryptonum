{-# LANGUAGE QuasiQuotes #-}
module Add(
    safeAddOps
  , unsafeAddOps
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

safeAddOps :: File
safeAddOps = File {
    predicate = \ me others -> (me + 64) `elem` others,
    outputName = "safe_add",
    isUnsigned = True,
    generator = declareSafeAddOperators,
    testCase = Just generateSafeTests
}

unsafeAddOps :: File
unsafeAddOps = File {
    predicate = \ _ _ -> True,
    outputName = "unsafe_add",
    isUnsigned = True,
    generator = declareUnsafeAddOperators,
    testCase = Just generateUnsafeTests
}

declareSafeAddOperators :: Word -> SourceFile Span
declareSafeAddOperators bitsize =
  let sname = mkIdent ("U" ++ show bitsize)
      dname = mkIdent ("U" ++ show (bitsize + 64))
      fullRippleAdd = makeRippleAdder True (bitsize `div` 64) "res"
      testFileLit = Lit [] (Str (testFile bitsize) Cooked Unsuffixed mempty) mempty
  in [sourceFile|
        use core::ops::Add;
        use crate::CryptoNum;
        #[cfg(test)]
        use crate::testing::{build_test_path,run_test};
        #[cfg(test)]
        use quickcheck::quickcheck;
        use crate::unsigned::{$$sname,$$dname};

        impl Add for $$sname {
          type Output = $$dname;

          fn add(self, rhs: $$sname) -> $$dname {
             &self + &rhs
          }
        }

        impl<'a> Add<&'a $$sname> for $$sname {
          type Output = $$dname;

          fn add(self, rhs: &$$sname) -> $$dname {
            &self + rhs
          }
        }

        impl<'a> Add<$$sname> for &'a $$sname {
          type Output = $$dname;

          fn add(self, rhs: $$sname) -> $$dname {
            self + &rhs
          }
        }

        impl<'a,'b> Add<&'a $$sname> for &'b $$sname {
          type Output = $$dname;

          fn add(self, rhs: &$$sname) -> $$dname {
            let mut res = $$dname::zero();

            $@{fullRippleAdd}

            res
          }
        }

        #[cfg(test)]
        quickcheck! {
           fn addition_symmetric(a: $$sname, b: $$sname) -> bool {
             (&a + &b) == (&b + &a)
           }
        }

       #[cfg(test)]
       #[allow(non_snake_case)]
       #[test]
       fn KATs() {
         run_test(build_test_path("safe_add", $$(testFileLit)), 3, |case| {
           let (neg0, xbytes) = case.get("x").unwrap();
           let (neg1, ybytes) = case.get("y").unwrap();
           let (neg2, zbytes) = case.get("z").unwrap();

           assert!(!neg0 && !neg1 && !neg2);
           let x = $$sname::from_bytes(&xbytes);
           let y = $$sname::from_bytes(&ybytes);
           let z = $$dname::from_bytes(&zbytes);

           assert_eq!(z, x + y);
        });
      }
      |]

declareUnsafeAddOperators :: Word -> SourceFile Span
declareUnsafeAddOperators bitsize =
  let sname = mkIdent ("U" ++ show bitsize)
      fullRippleAdd = makeRippleAdder False (bitsize `div` 64) "self"
      testFileLit = Lit [] (Str (testFile bitsize) Cooked Unsuffixed mempty) mempty
  in [sourceFile|
        use core::ops::AddAssign;
        #[cfg(test)]
        use crate::CryptoNum;
        #[cfg(test)]
        use crate::testing::{build_test_path,run_test};
        #[cfg(test)]
        use quickcheck::quickcheck;
        use crate::unsigned::$$sname;

        impl AddAssign for $$sname {
          fn add_assign(&mut self, rhs: Self) {
            self.add_assign(&rhs);
          }
        }

        impl<'a> AddAssign<&'a $$sname> for $$sname {
          fn add_assign(&mut self, rhs: &Self) {
            $@{fullRippleAdd}
          }
        }

        #[cfg(test)]
        quickcheck! {
           fn addition_symmetric(a: $$sname, b: $$sname) -> bool {
             let mut side1 = a.clone();
             let mut side2 = b.clone();

             side1 += b;
             side2 += a;

             side1 == side2
           }
        }

       #[cfg(test)]
       #[allow(non_snake_case)]
       #[test]
       fn KATs() {
         run_test(build_test_path("unsafe_add", $$(testFileLit)), 3, |case| {
           let (neg0, xbytes) = case.get("x").unwrap();
           let (neg1, ybytes) = case.get("y").unwrap();
           let (neg2, zbytes) = case.get("z").unwrap();

           assert!(!neg0 && !neg1 && !neg2);
           let mut x = $$sname::from_bytes(&xbytes);
           let y     = $$sname::from_bytes(&ybytes);
           let z     = $$sname::from_bytes(&zbytes);

           x += &y;
           assert_eq!(z, x);
        });
      }
      |]


makeRippleAdder :: Bool -> Word -> String -> [Stmt Span]
makeRippleAdder useLastCarry inElems resName =
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
  , [stmt|let $$right = rhs.value[$$(liti)] as u128; |]
  , if i == 0
      then [stmt| let $$sumi = $$left + $$right; |]
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
      in [[stmt| $$target.value[$$(liti)] = $$res as u64; |]]
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
    let (x, g1) = generateNum g0 size
        (y, g2) = generateNum g1 size
        tcase   = Map.fromList [("x", showX x), ("y", showX y),
                                ("z", showX (x + y))]
    in tcase : go g2 (i - 1)

generateUnsafeTests :: RandomGen g => Word -> g -> [Map String String]
generateUnsafeTests size g = go g numTestCases
 where
  go _  0 = []
  go g0 i =
    let (x, g1) = generateNum g0 size
        (y, g2) = generateNum g1 size
        z       = (x + y) .&. ((2 ^ size) - 1)
        tcase   = Map.fromList [("x", showX x), ("y", showX y),
                                ("z", showX z)]
    in tcase : go g2 (i - 1)
