module BinaryOps(
    binaryOps
  )
 where

import Control.Monad(forM_)
import File
import Gen

binaryOps :: File
binaryOps = File {
    predicate = \ _ _ -> True,
    outputName = "binary",
    generator = declareBinaryOperators
}

declareBinaryOperators :: Word -> Gen ()
declareBinaryOperators bitsize =
  do let name = "U" ++ show bitsize
         entries = bitsize `div` 64
     out "use core::ops::{BitAnd,BitAndAssign};"
     out "use core::ops::{BitOr,BitOrAssign};"
     out "use core::ops::{BitXor,BitXorAssign};"
     out "use core::ops::Not;"
     out ("use super::U" ++ show bitsize ++ ";")
     blank
     generateBinOps "BitAnd" name "bitand" "&=" entries
     blank
     generateBinOps "BitOr"  name "bitor"  "|=" entries
     blank
     generateBinOps "BitXor" name "bitxor" "^=" entries
     blank
     implFor "Not" name $
       do out "type Output = Self;"
          blank
          wrapIndent "fn not(mut self) -> Self" $
            do forM_ [0..entries-1] $ \ i ->
                 out ("self.value[" ++ show i ++ "] = !self.value[" ++ show i ++ "];")
               out "self"
     blank
     implFor' "Not" ("&'a " ++ name) $
       do out ("type Output = " ++ name ++ ";")
          blank
          wrapIndent ("fn not(self) -> " ++ name) $
            do out "let mut output = self.clone();"
               forM_ [0..entries-1] $ \ i ->
                 out ("output.value[" ++ show i ++ "] = !self.value[" ++ show i ++ "];")
               out "output"

generateBinOps :: String -> String -> String -> String -> Word -> Gen ()
generateBinOps trait name fun op entries =
  do implFor (trait ++ "Assign") name $
       wrapIndent ("fn " ++ fun ++ "_assign(&mut self, rhs: Self)") $
         forM_ [0..entries-1] $ \ i ->
           out ("self.value[" ++ show i ++ "] "++op++" rhs.value[" ++ show i ++ "];")
     blank
     implFor' (trait ++ "Assign<&'a " ++ name ++ ">") name $
       wrapIndent ("fn " ++ fun ++ "_assign(&mut self, rhs: &Self)") $
         forM_ [0..entries-1] $ \ i ->
           out ("self.value[" ++ show i ++ "] "++op++" rhs.value[" ++ show i ++ "];")
     blank
     generateBinOpsFromAssigns trait name fun op

generateBinOpsFromAssigns :: String -> String -> String -> String -> Gen ()
generateBinOpsFromAssigns trait name fun op =
  do implFor trait name $
       do out "type Output = Self;"
          blank
          wrapIndent ("fn " ++ fun ++ "(mut self, rhs: Self) -> Self") $
            do out ("self " ++ op ++ " rhs;")
               out "self"
     blank
     implFor' (trait ++ "<&'a " ++ name ++ ">") name $
       do out "type Output = Self;"
          blank
          wrapIndent ("fn " ++ fun ++ "(mut self, rhs: &Self) -> Self") $
            do out ("self " ++ op ++ " rhs;")
               out "self"
     blank
     implFor' (trait ++ "<" ++ name ++ ">") ("&'a " ++ name) $
       do out ("type Output = " ++ name ++ ";")
          blank
          wrapIndent ("fn " ++ fun ++ "(self, mut rhs: " ++ name ++ ") -> " ++ name) $
            do out ("rhs " ++ op ++ " self;")
               out "rhs"
     blank
     implFor'' (trait ++ "<&'a " ++ name ++ ">") ("&'b " ++ name) $
       do out ("type Output = " ++ name ++ ";")
          blank
          wrapIndent ("fn " ++ fun ++ "(self, rhs: &" ++ name ++ ") -> " ++ name) $
            do out "let mut output = self.clone();"
               out ("output " ++ op ++ " rhs;")
               out "output"