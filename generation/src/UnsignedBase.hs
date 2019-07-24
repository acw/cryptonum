module UnsignedBase(
    base
  )
 where

import Control.Monad(forM_)
import Data.List(intercalate)
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
     out "use core::cmp::{Eq,Ordering,PartialEq,min};"
     out "use core::fmt;"
     out "use super::super::super::CryptoNum;"
     blank
     out "#[derive(Clone)]"
     wrapIndent ("pub struct " ++ name) $
       out ("pub(crate) value: [u64; " ++ show entries ++ "]")
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
          wrapIndent ("fn from_bytes(&self, bytes: &[u8]) -> Self") $
            do let bytes = bitsize `div` 8;
               forM_ [0..bytes-1] $ \ idx ->
                 out ("let byte" ++ show idx ++ " = " ++
                      "if " ++ show idx ++ " < bytes.len() { " ++
                      "bytes[" ++ show idx ++ "] as u64 } else { 0 };")
               blank
               let byteNames = map (\ x -> "byte" ++ show x) [0..bytes-1]
                   byteGroups = groupCount 8 (reverse byteNames)
               forM_ (zip byteGroups [0..bytes-1]) $ \ (byteGroup, idx) ->
                 do let shiftAmts = [0,8..56]
                        shifts = zipWith (\ n s -> n ++ " << " ++ show s)
                                         byteGroup shiftAmts
                        shift0 = head shifts
                        shiftL = last shifts
                        middles = reverse (drop 1 (reverse (drop 1 shifts)))
                        prefix = "let word" ++ show idx ++ " "
                        blankPrefix = map (const ' ') prefix
                    out (prefix ++ " = " ++ shift0)
                    forM_ middles $ \ s -> out (blankPrefix ++ " | " ++ s)
                    out (blankPrefix ++ " | " ++ shiftL ++ ";")
               blank
               wrapIndent name $
                 do out ("value: [")
                    let vwords = map (\ x -> "word" ++ show x) [0..top]
                        linewords = groupCount 4 vwords
                        vlines = map (intercalate ", ") linewords
                    forM_ vlines $ \ l -> out ("    " ++ l ++ ",")
                    out ("]")
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
     blank
     out "#[test]"
     wrapIndent "fn KATs()" $
       do out ("run_test(\"testdata/base/" ++ name ++ ".test\", 8, |case| {")
          indent $
            do out ("let (neg0, xbytes) = case.get(\"x\").unwrap();")
               out ("let (neg1, mbytes) = case.get(\"m\").unwrap();")
               out ("let (neg2, zbytes) = case.get(\"z\").unwrap();")
               out ("let (neg3, ebytes) = case.get(\"e\").unwrap();")
               out ("let (neg4, obytes) = case.get(\"o\").unwrap();")
               out ("let (neg5, rbytes) = case.get(\"r\").unwrap();")
               out ("let (neg6, bbytes) = case.get(\"b\").unwrap();")
               out ("let (neg7, tbytes) = case.get(\"t\").unwrap();")
               out ("assert!(!neg0&&!neg1&&!neg2&&!neg3&&!neg4&&!neg5&&!neg6&&!neg7);")
               out ("let mut x = "++name++"::from_bytes(xbytes);")
               out ("let m = "++name++"::from_bytes(mbytes);")
               out ("let z = 1 == zbytes[0];")
               out ("let e = 1 == ebytes[0];")
               out ("let o = 1 == obytes[0];")
               out ("let r = "++name++"::from_bytes(rbytes);")
               out ("let b = usize::from("++name++"::from_bytes(bbytes));")
               out ("let t = 1 == tbytes[0];")
               out ("assert_eq!(x.is_zero(),  z);")
               out ("assert_eq!(x.is_even(),  e);")
               out ("assert_eq!(x.is_odd(),   o);")
               out ("assert_eq!(x.testbit(b), t);")
               out ("x.mask(usize::from(&m));")
               out ("assert_eq!(x, r);")
          out ("});")

groupCount :: Int -> [a] -> [[a]]
groupCount x ls
  | x >= length ls = [ls]
  | otherwise     = take x ls : groupCount x (drop x ls)