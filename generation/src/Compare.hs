{-# LANGUAGE QuasiQuotes #-}
module Compare(comparisons)
 where

import File
import Language.Rust.Data.Ident
import Language.Rust.Data.Position
import Language.Rust.Quote
import Language.Rust.Syntax

comparisons :: File
comparisons = File {
  predicate = \ _ _ -> True,
  outputName = "compare",
  isUnsigned = True,
  generator = declareComparators,
  testCase = Nothing
}

declareComparators :: Word -> SourceFile Span
declareComparators bitsize =
  let sname = mkIdent ("U" ++ show bitsize)
      entries = bitsize `div` 64
      eqStatements = buildEqStatements 0 entries
      compareExp = buildCompareExp 0 entries
  in [sourceFile|
       use core::cmp::{Eq,Ordering,PartialEq};
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
