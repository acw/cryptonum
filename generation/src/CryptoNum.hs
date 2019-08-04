module CryptoNum(
    cryptoNum
  )
 where

import Control.Monad(forM_)
import File
import Gen

cryptoNum :: File
cryptoNum = File {
  predicate = \ _ _ -> True,
  outputName = "cryptonum",
  generator = declareCryptoNumInstance
}

declareCryptoNumInstance :: Word -> Gen ()
declareCryptoNumInstance bitsize =
  do let name = "U" ++ show bitsize
         entries = bitsize `div` 64
         top = entries - 1
     out "use core::cmp::min;"
     out "use crate::CryptoNum;"
     out "#[cfg(test)]"
     out "use crate::testing::{build_test_path,run_test};"
     out "#[cfg(test)]"
     out "use quickcheck::{Arbitrary,Gen,quickcheck};"
     out "#[cfg(test)]"
     out "use std::fmt;"
     out ("use super::" ++ name ++ ";")
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
            out "self.value[0] & 0x1 == 1"
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
          wrapIndent ("fn from_bytes(bytes: &[u8]) -> Self") $
            do out ("let biggest = min(" ++ show (bitsize `div` 8) ++ ", " ++
                    "bytes.len()) - 1;")
               out ("let mut idx = biggest / 8;")
               out ("let mut shift = (biggest % 8) * 8;")
               out ("let mut i = 0;")
               out ("let mut res = " ++ name ++ "::zero();")
               blank
               wrapIndent ("while i <= biggest") $
                 do out ("res.value[idx] |= (bytes[i] as u64) << shift;")
                    out ("i += 1;")
                    out ("if shift == 0 {")
                    indent $
                      do out "shift = 56;"
                         out "if idx > 0 { idx -= 1; }"
                    out ("} else {")
                    indent $
                      out "shift -= 8;"
                    out "}"
               blank
               out "res"
          blank
          wrapIndent ("fn to_bytes(&self, bytes: &mut [u8])") $
            do let bytes = bitsize `div` 8
               out ("if bytes.len() == 0 { return; }")
               blank
               forM_ [0..bytes-1] $ \ idx ->
                 do let (validx, shift) = byteShiftInfo idx
                    out ("let byte" ++ show idx ++ " = (self.value[" ++
                         show validx ++ "] >> " ++ show shift ++ ")" ++
                         " as u8;")
               blank
               out ("let mut idx = min(bytes.len() - 1, " ++ show (bytes - 1) ++ ");")
               forM_ [0..bytes-2] $ \ i ->
                 do out ("bytes[idx] = byte" ++ show i ++ ";")
                    out ("if idx == 0 { return; }")
                    out ("idx -= 1;")
               out ("bytes[idx] = byte" ++ show (bytes-1) ++ ";")
     blank
     let bytes = bitsize `div` 8
         struct = "Bytes" ++ show bytes
     out "#[cfg(test)]"
     out "#[derive(Clone)]"
     wrapIndent ("struct " ++ struct) $
       out ("value: [u8; " ++ show bytes ++ "]")
     blank
     out "#[cfg(test)]"
     implFor "PartialEq" struct $
       wrapIndent ("fn eq(&self, other: &Self) -> bool") $
         out "self.value.iter().zip(other.value.iter()).all(|(a,b)| a == b)"
     blank
     out "#[cfg(test)]"
     implFor "fmt::Debug" struct $
       wrapIndent ("fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result") $
         out "f.debug_list().entries(self.value.iter()).finish()"
     blank
     out "#[cfg(test)]"
     implFor "Arbitrary" struct $
       wrapIndent ("fn arbitrary<G: Gen>(g: &mut G) -> Self") $
         do out ("let mut res = " ++ struct ++ "{ value: [0; " ++ show bytes ++ "] };")
            out ("g.fill_bytes(&mut res.value);")
            out ("res")
     blank
     out "#[cfg(test)]"
     wrapIndent "quickcheck!" $
       do wrapIndent ("fn to_from_ident(x: " ++ name ++ ") -> bool") $
            do out ("let mut buffer = [0; " ++ show bytes ++ "];")
               out ("x.to_bytes(&mut buffer);");
               out ("let y = " ++ name ++ "::from_bytes(&buffer);")
               out ("x == y")
          blank
          wrapIndent ("fn from_to_ident(x: " ++ struct ++ ") -> bool") $
            do out ("let val = " ++ name ++ "::from_bytes(&x.value);")
               out ("let mut buffer = [0; " ++ show bytes ++ "];")
               out ("val.to_bytes(&mut buffer);")
               out ("buffer.iter().zip(x.value.iter()).all(|(a,b)| a == b)")
     blank
     out "#[cfg(test)]"
     out "#[allow(non_snake_case)]"
     out "#[test]"
     wrapIndent "fn KATs()" $
       do let name' = pad 5 '0' (show bitsize)
          out ("run_test(build_test_path(\"base\",\"" ++ name' ++ "\"), 8, |case| {")
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

byteShiftInfo :: Word -> (Word, Word)
byteShiftInfo idx =
    (idx `div` 8, (idx `mod` 8) * 8)

pad :: Int -> Char -> String -> String
pad len c str
  | length str >= len = str
  | otherwise         = pad len c (c:str)