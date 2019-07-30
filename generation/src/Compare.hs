module Compare(comparisons)
 where

import Control.Monad(forM_)
import File
import Gen

comparisons :: File
comparisons = File {
  predicate = \ _ _ -> True,
  outputName = "compare",
  generator = declareComparators
}

declareComparators :: Word -> Gen ()
declareComparators bitsize =
  do let name = "U" ++ show bitsize
         entries = bitsize `div` 64
         top = entries - 1
     out "use core::cmp::{Eq,Ordering,PartialEq};"
     out "#[cfg(test)]"
     out "use quickcheck::quickcheck;"
     out ("use super::" ++ name ++ ";")
     blank
     implFor "PartialEq" name $
       wrapIndent "fn eq(&self, other: &Self) -> bool" $
         do forM_ (reverse [1..top]) $ \ i ->
              out ("self.value[" ++ show i ++ "] == other.value[" ++ show i ++ "] && ")
            out "self.value[0] == other.value[0]"
     blank
     implFor "Eq" name $ return ()
     blank
     implFor "Ord" name $
       wrapIndent "fn cmp(&self, other: &Self) -> Ordering" $
         do out ("self.value[" ++ show top ++ "].cmp(&other.value[" ++ show top ++ "])") 
            forM_ (reverse [0..top-1]) $ \ i ->
              out ("  .then(self.value[" ++ show i ++ "].cmp(&other.value[" ++ show i ++ "]))")
     blank
     implFor "PartialOrd" name $
       wrapIndent "fn partial_cmp(&self, other: &Self) -> Option<Ordering>" $
         out "Some(self.cmp(other))"
     blank
     out "#[cfg(test)]"
     wrapIndent "quickcheck!" $
       do let transFun n = "fn " ++ n ++ "(a: " ++ name ++ ", b: " ++ name ++
                           ", c: " ++ name ++ ") -> bool"
          wrapIndent (transFun "eq_is_transitive") $
            out ("if a == c { a == b && b == c } else { a != b || b != c }")
          blank
          wrapIndent (transFun "gt_is_transitive") $
            out ("if a > b && b > c { a > c } else { true }")
          blank
          wrapIndent (transFun "ge_is_transitive") $
            out ("if a >= b && b >= c { a >= c } else { true }")
          blank
          wrapIndent (transFun "lt_is_transitive") $
            out ("if a < b && b < c { a < c } else { true }")
          blank
          wrapIndent (transFun "le_is_transitive") $
            out ("if a <= b && b <= c { a <= c } else { true }")