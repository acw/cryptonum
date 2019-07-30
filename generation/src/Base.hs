module Base(
    base
  )
 where

import Control.Monad(forM_)
import File
import Gen

base :: File
base = File {
  predicate = \ _ _ -> True,
  outputName = "base",
  generator = declareBaseStructure
}

declareBaseStructure :: Word -> Gen ()
declareBaseStructure bitsize =
  do let name = "U" ++ show bitsize
         entries = bitsize `div` 64
         top = entries - 1
     out "use core::fmt;"
     out "use quickcheck::{Arbitrary,Gen};"
     blank
     out "#[derive(Clone)]"
     wrapIndent ("pub struct " ++ name) $
       out ("pub(crate) value: [u64; " ++ show entries ++ "]")
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
     blank
     implFor "Arbitrary" name $
       wrapIndent "fn arbitrary<G: Gen>(g: &mut G) -> Self" $
         do out (name ++ " {")
            indent $
              do out ("value: [")
                 indent $ forM_ [0..top] $ \ _ ->
                   out ("g.next_u64(),")
                 out ("]")
            out ("}")