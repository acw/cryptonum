module UnsignedBase(
    declareBaseStructure
  , declareBinaryOperators
  )
 where

import Control.Monad(forM_)
import Gen
import Requirements(Operation)

declareBaseStructure :: Int -> [Operation] -> Gen ()
declareBaseStructure bitsize ops =
  do let name = "U" ++ show bitsize
         entries = bitsize `div` 64
         top = entries - 1
     out "use core::cmp::{Eq,Ordering,PartialEq,min};"
     out "use core::fmt;"
     out "use super::super::CryptoNum;"
     blank
     out "mod binary;"
     blank
     out "#[derive(Clone)]"
     wrapIndent ("pub struct " ++ name) $
       out ("value: [u64; " ++ show entries ++ "]")
     blank
     implFor "CryptoNum" name $
       do wrapIndent ("fn zero() -> Self") $
            out (name ++ "{ value: [0; " ++ show entries ++ "] }")
          blank
          wrapIndent ("fn is_zero(&self) -> bool") $
            do forM_ (reverse [1..top]) $ \ i ->
                 out ("self.value[" ++ show i ++ "] == 0 &&")
               out "self.value[0] == 0"
          blank
          wrapIndent ("fn is_even(&self) -> bool") $
            out "self.value[0] & 0x1 == 0"
          blank
          wrapIndent ("fn is_odd(&self) -> bool") $
            out "self.value[0] & 0x1 == 0"
          blank
          wrapIndent ("fn bit_length() -> usize") $
            out (show bitsize)
          blank
          wrapIndent ("fn mask(&mut self, len: usize)") $
            do out ("let dellen = min(len, " ++ show entries ++ ");")
               wrapIndent ("for i in dellen.." ++ show entries) $
                 out ("self.value[i] = 0;")
          blank
          wrapIndent ("fn testbit(&self, bit: usize) -> bool") $
            do out "let idx = bit / 64;"
               out "let offset = bit % 64;"
               wrapIndent ("if idx >= " ++ show entries) $
                 out "return false;"
               out "(self.value[idx] & (1u64 << offset)) != 0"
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
     implFor "fmt::Debug" name $
       wrapIndent "fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result" $
         do out ("f.debug_tuple(" ++ show name ++ ")")
            forM_ [0..top] $ \ i ->
              out (" .field(&self.value[" ++ show i ++ "])")
            out " .finish()"
     blank
     implFor "fmt::UpperHex" name $
       wrapIndent "fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result" $
         do forM_ (reverse [1..top]) $ \ i ->
              out ("write!(f, \"{:X}\", self.value[" ++ show i ++ "])?;")
            out "write!(f, \"{:X}\", self.value[0])"
     blank
     implFor "fmt::LowerHex" name $
       wrapIndent "fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result" $
         do forM_ (reverse [1..top]) $ \ i ->
              out ("write!(f, \"{:x}\", self.value[" ++ show i ++ "])?;")
            out "write!(f, \"{:x}\", self.value[0])"

declareBinaryOperators :: Int -> Gen ()
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

generateBinOps :: String -> String -> String -> String -> Int -> Gen ()
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
