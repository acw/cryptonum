module UnsignedBase(
    declareBaseStructure
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
     out "mod binary_ops;"
     blank
     wrapIndent ("pub struct " ++ name) $
       out ("value: [u64; " ++ show entries ++ "]")
     blank
     wrapIndent ("impl CryptoNum for " ++ name) $
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
     wrapIndent ("impl PartialEq for " ++ name) $
       wrapIndent "fn eq(&self, other: &Self) -> bool" $
         do forM_ (reverse [1..top]) $ \ i ->
              out ("self.value[" ++ show i ++ "] == other.value[" ++ show i ++ "] && ")
            out "self.value[0] == other.value[0]"
     blank
     out ("impl Eq for " ++ name ++ " {}")
     blank
     wrapIndent ("impl Ord for " ++ name) $
       wrapIndent "fn cmp(&self, other: &Self) -> Ordering" $
         do out ("self.value[" ++ show top ++ "].cmp(&other.value[" ++ show top ++ "])") 
            forM_ (reverse [0..top-1]) $ \ i ->
              out ("  .then(self.value[" ++ show i ++ "].cmp(&other.value[" ++ show i ++ "]))")
     blank
     wrapIndent ("impl PartialOrd for " ++ name) $
       wrapIndent "fn partial_cmp(&self, other: &Self) -> Option<Ordering>" $
         out "Some(self.cmp(other))"
     blank
     wrapIndent ("impl fmt::Debug for " ++ name) $
       wrapIndent "fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result" $
         do out ("f.debug_tuple(" ++ show name ++ ")")
            forM_ [0..top] $ \ i ->
              out (" .field(&self.value[" ++ show i ++ "])")
            out " .finish()"
     blank
     wrapIndent ("impl fmt::UpperHex for " ++ name) $
       wrapIndent "fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result" $
         do forM_ (reverse [1..top]) $ \ i ->
              out ("write!(f, \"{:X}\", self.value[" ++ show i ++ "])?;")
            out "write!(f, \"{:X}\", self.value[0])"
     blank
     wrapIndent ("impl fmt::LowerHex for " ++ name) $
       wrapIndent "fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result" $
         do forM_ (reverse [1..top]) $ \ i ->
              out ("write!(f, \"{:x}\", self.value[" ++ show i ++ "])?;")
            out "write!(f, \"{:x}\", self.value[0])"