{-# LANGUAGE QuasiQuotes #-}
module Compare(comparisons, signedComparisons)
 where

import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import File
import Generators
import Language.Rust.Data.Ident
import Language.Rust.Data.Position
import Language.Rust.Quote
import Language.Rust.Syntax
import System.Random(RandomGen)

numTestCases :: Int
numTestCases = 3000

comparisons :: File
comparisons = File {
  predicate = \ _ _ -> True,
  outputName = "compare",
  isUnsigned = True,
  generator = declareComparators,
  testCase = Just generateTests
}

signedComparisons :: File
signedComparisons = File {
  predicate = \ _ _ -> True,
  outputName = "scompare",
  isUnsigned = False,
  generator = declareSignedComparators,
  testCase = Just generateSignedTests
}

declareComparators :: Word -> [Word] -> SourceFile Span
declareComparators bitsize _ =
  let sname = mkIdent ("U" ++ show bitsize)
      entries = bitsize `div` 64
      eqStatements = buildEqStatements 0 entries
      compareExp = buildCompareExp 0 entries
      testFileLit = Lit [] (Str (testFile True bitsize) Cooked Unsuffixed mempty) mempty
  in [sourceFile|
       use core::cmp::{Eq,Ordering,PartialEq};
       #[cfg(test)]
       use crate::CryptoNum;
       #[cfg(test)]
       use crate::testing::{build_test_path,run_test};
       #[cfg(test)]
       use quickcheck::quickcheck;
       use super::$$sname;

       impl PartialEq for $$sname {
         fn eq(&self, other: &Self) -> bool {
           let mut out = true;
           $@{eqStatements}
           out
         }
       }

       impl Eq for $$sname {}

       impl Ord for $$sname {
         fn cmp(&self, other: &Self) -> Ordering {
            $$(compareExp)
         }
       }

       impl PartialOrd for $$sname {
         fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
           Some(self.cmp(other))
         }
       }

       #[cfg(test)]
       quickcheck! {
         fn eq_is_transitive(a: $$sname, b: $$sname, c: $$sname) -> bool {
           if a == c { a == b && b == c } else { a != b || b != c }
         }

         fn gt_is_transitive(a: $$sname, b: $$sname, c: $$sname) -> bool {
           if a > b && b > c { a > c } else { true }
         }

         fn ge_is_transitive(a: $$sname, b: $$sname, c: $$sname) -> bool {
           if a >= b && b >= c { a >= c } else { true }
         }

         fn lt_is_transitive(a: $$sname, b: $$sname, c: $$sname) -> bool {
           if a < b && b < c { a < c } else { true }
         }

         fn le_is_transitive(a: $$sname, b: $$sname, c: $$sname) -> bool {
           if a <= b && b <= c { a <= c } else { true }
         }
       }

       #[cfg(test)]
       #[allow(non_snake_case)]
       #[test]
       fn KATs() {
         run_test(build_test_path("compare", $$(testFileLit)), 8, |case| {
           let (neg0, xbytes) = case.get("x").unwrap();
           let (neg1, ybytes) = case.get("y").unwrap();
           let (neg2, ebytes) = case.get("e").unwrap();
           let (neg3, nbytes) = case.get("n").unwrap();
           let (neg4, gbytes) = case.get("g").unwrap();
           let (neg5, hbytes) = case.get("h").unwrap();
           let (neg6, lbytes) = case.get("l").unwrap();
           let (neg7, kbytes) = case.get("k").unwrap();

           assert!(!neg0 && !neg1 && !neg2 && !neg3 &&
                   !neg4 && !neg5 && !neg6 && !neg7);
           let x = $$sname::from_bytes(&xbytes);
           let y = $$sname::from_bytes(&ybytes);
           let e = 1 == ebytes[0];
           let n = 1 == nbytes[0];
           let g = 1 == gbytes[0];
           let h = 1 == hbytes[0];
           let l = 1 == lbytes[0];
           let k = 1 == kbytes[0];

           assert_eq!(e, x == y);
           assert_eq!(n, x != y);
           assert_eq!(g, x >  y);
           assert_eq!(h, x >= y);
           assert_eq!(l, x <  y);
           assert_eq!(k, x <= y);
         });
       }
    |]

declareSignedComparators :: Word -> [Word] -> SourceFile Span
declareSignedComparators bitsize _ =
  let sname = mkIdent ("I" ++ show bitsize)
      entries = bitsize `div` 64
      eqStatements = buildEqStatements 0 entries
      compareExp = buildCompareExp 0 entries
      testFileLit = Lit [] (Str (testFile True bitsize) Cooked Unsuffixed mempty) mempty
  in [sourceFile|
       use core::cmp::{Eq,Ordering,PartialEq};
       #[cfg(test)]
       use crate::CryptoNum;
       #[cfg(test)]
       use crate::testing::{build_test_path,run_test};
       #[cfg(test)]
       use quickcheck::quickcheck;
       use super::$$sname;

       impl PartialEq for $$sname {
         fn eq(&self, other: &Self) -> bool {
           &self.contents == &other.contents
         }
       }

       impl Eq for $$sname {}

       impl Ord for $$sname {
         fn cmp(&self, other: &Self) -> Ordering {
           panic!("cmp")
         }
       }

       impl PartialOrd for $$sname {
         fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
           Some(self.cmp(other))
         }
       }

       #[cfg(test)]
       quickcheck! {
         fn eq_is_transitive(a: $$sname, b: $$sname, c: $$sname) -> bool {
           if a == c { a == b && b == c } else { a != b || b != c }
         }

         fn gt_is_transitive(a: $$sname, b: $$sname, c: $$sname) -> bool {
           if a > b && b > c { a > c } else { true }
         }

         fn ge_is_transitive(a: $$sname, b: $$sname, c: $$sname) -> bool {
           if a >= b && b >= c { a >= c } else { true }
         }

         fn lt_is_transitive(a: $$sname, b: $$sname, c: $$sname) -> bool {
           if a < b && b < c { a < c } else { true }
         }

         fn le_is_transitive(a: $$sname, b: $$sname, c: $$sname) -> bool {
           if a <= b && b <= c { a <= c } else { true }
         }
       }

       #[cfg(test)]
       #[allow(non_snake_case)]
       #[test]
       fn KATs() {
         run_test(build_test_path("scompare", $$(testFileLit)), 8, |case| {
           let (neg0, xbytes) = case.get("x").unwrap();
           let (neg1, ybytes) = case.get("y").unwrap();
           let (neg2, ebytes) = case.get("e").unwrap();
           let (neg3, nbytes) = case.get("n").unwrap();
           let (neg4, gbytes) = case.get("g").unwrap();
           let (neg5, hbytes) = case.get("h").unwrap();
           let (neg6, lbytes) = case.get("l").unwrap();
           let (neg7, kbytes) = case.get("k").unwrap();

           assert!(!neg0 && !neg1 && !neg2 && !neg3 &&
                   !neg4 && !neg5 && !neg6 && !neg7);
           let x = $$sname::from_bytes(&xbytes);
           let y = $$sname::from_bytes(&ybytes);
           let e = 1 == ebytes[0];
           let n = 1 == nbytes[0];
           let g = 1 == gbytes[0];
           let h = 1 == hbytes[0];
           let l = 1 == lbytes[0];
           let k = 1 == kbytes[0];

           assert_eq!(e, x == y);
           assert_eq!(n, x != y);
           assert_eq!(g, x >  y);
           assert_eq!(h, x >= y);
           assert_eq!(l, x <  y);
           assert_eq!(k, x <= y);
         });
       }
    |]

buildEqStatements :: Word -> Word -> [Stmt Span]
buildEqStatements i numEntries
  | i == (numEntries - 1) =
       [[stmt| out &= self.value[$$(x)] == other.value[$$(x)]; |]]
  | otherwise =
      let rest = buildEqStatements (i + 1) numEntries
          cur  = [stmt| out &= self.value[$$(x)] == other.value[$$(x)]; |]
      in cur:rest
 where
  x = Lit [] (Int Dec (fromIntegral i) Unsuffixed mempty) mempty

buildCompareExp :: Word -> Word -> Expr Span
buildCompareExp i numEntries
  | i == (numEntries - 1) =
      [expr| self.value[$$(x)].cmp(&other.value[$$(x)]) |]
  | otherwise =
      let rest = buildCompareExp (i + 1) numEntries
      in [expr| $$(rest).then(self.value[$$(x)].cmp(&other.value[$$(x)])) |]
 where
  x = Lit [] (Int Dec (fromIntegral i) Unsuffixed mempty) mempty

generateTests :: RandomGen g => Word -> g -> [Map String String]
generateTests size g = go g numTestCases
 where
  go _  0 = []
  go g0 i =
   let (x, g1) = generateNum g0 size
       (y, g2) = generateNum g1 size
       tcase   = Map.fromList [("x", showX x), ("y", showX y),
                               ("e", showB (x == y)),
                               ("n", showB (x /= y)),
                               ("g", showB (x >  y)),
                               ("h", showB (x >= y)),
                               ("l", showB (x <  y)),
                               ("k", showB (x <= y))]
   in tcase : go g2 (i - 1)

generateSignedTests :: RandomGen g => Word -> g -> [Map String String]
generateSignedTests size g = go g numTestCases
 where
  go _  0 = []
  go g0 i =
   let (x, g1) = generateSignedNum g0 size
       (y, g2) = generateSignedNum g1 size
       tcase   = Map.fromList [("x", showX x), ("y", showX y),
                               ("e", showB (x == y)),
                               ("n", showB (x /= y)),
                               ("g", showB (x >  y)),
                               ("h", showB (x >= y)),
                               ("l", showB (x <  y)),
                               ("k", showB (x <= y))]
   in tcase : go g2 (i - 1)
