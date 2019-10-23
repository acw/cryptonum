{-# LANGUAGE QuasiQuotes #-}
module CryptoNum(
    cryptoNum
  )
 where

import Control.Monad(forM_)
import File
import Gen
import Language.Rust.Data.Ident
import Language.Rust.Data.Position
import Language.Rust.Quote
import Language.Rust.Pretty
import Language.Rust.Syntax

cryptoNum :: File
cryptoNum = File {
  predicate = \ _ _ -> True,
  outputName = "cryptonum",
  generator = declareCryptoNumInstance
}

declareCryptoNumInstance :: Word -> Gen ()
declareCryptoNumInstance bitsize =
  do let sname = mkIdent ("U" ++ show bitsize)
         entries = bitsize `div` 64
         entlit = Lit [] (Int Dec (fromIntegral entries) Unsuffixed mempty) mempty
         top = entries - 1
         zeroTests = generateZeroTests 0 entries
         bitlength = toLit bitsize
         bytelen = bitsize `div` 8
         bytelenlit = toLit bytelen
         bytebuffer = Delimited mempty Brace (Stream [
                        Tree (Token mempty (LiteralTok (IntegerTok "0") Nothing)),
                        Tree (Token mempty Semicolon),
                        Tree (Token mempty (LiteralTok (IntegerTok (show bytelen)) Nothing))
                      ])
         entrieslit = toLit entries
         packerLines = generatePackerLines 0 (bitsize `div` 8)
     out $ show $ pretty' $ [sourceFile|
       use core::cmp::min;
       use crate::CryptoNum;
       #[cfg(test)]
       use crate::testing::{build_test_path,run_test};
       #[cfg(test)]
       use quickcheck::quickcheck;
       use super::$$sname;

       impl CryptoNum for $$sname {
         fn zero() -> Self {
           $$sname{ value: [0; $$(entlit)] }
         }
         fn is_zero(&self) -> bool {
           let mut result = true;
           $@{zeroTests}
           result
         }
         fn is_even(&self) -> bool {
           self.value[0] & 0x1 == 0
         }
         fn is_off(&self) -> bool {
           self.value[0] & 0x1 == 1
         }
         fn bit_length() -> usize {
           $$(bitlength)
         }
         fn mask(&mut self, len: usize) {
           let dellen = min(len, $$(entrieslit));
           for i in dellen..$$(entrieslit) {
             self.value[i] = 0;
           }
         }
         fn testbit(&self, bit: usize) -> bool {
           let idx = bit / 64;
           let offset = bit % 64;
           if idx >= $$(entrieslit) {
             return false;
           }
           (self.value[idx] & (1u64 << offset)) != 0
         }
         fn from_bytes(bytes: &[u8]) -> Self {
           let biggest = min($$(bytelenlit), bytes.len()) - 1;
           let mut idx = biggest / 8;
           let mut shift = (biggest % 8) * 8;
           let mut i = 0;
           let mut res = $$sname::zero();

           while i <= biggest {
             res.value[idx] |= (bytes[i] as u64) << shift;
             i += 1;
             if shift == 0 {
               shift = 56;
               if idx > 0 {
                 idx -= 1;
               }
             } else {
               shift -= 8;
             }
           }

           res
         }
         fn to_bytes(&self, bytes: &mut [u8]) {
            let mut idx = 0;
            let mut shift = 0;

            for x in bytes.iter_mut().take($$(bytelenlit)).reverse() {
               *x = (self.values[idx] >> shift) as u8;
               shift += 8;
               if shift == 64 {
                 idx += 1;
                 shift = 0;
               }
            }
         }
       }

       #[cfg(test)]
       quickcheck! {
         fn to_from_ident(x: $$sname) -> bool {
           let mut buffer = $$(bytebuffer);
           x.to_bytes(&mut buffer);
           let y = $$sname::from_bytes(&buffer);
           x == y
         }
       }

       #[cfg(test)]
       #[allow(non_snake_case)]
       #[test]
       fn KATs() {
         run_test(build_test_path("base", stringify!($$sname)), 8, |case| {
           let (neg0, xbytes) = case.get("x").unwrap();
           let (neg1, mbytes) = case.get("m").unwrap();
           let (neg2, zbytes) = case.get("z").unwrap();
           let (neg3, ebytes) = case.get("e").unwrap();
           let (neg4, obytes) = case.get("o").unwrap();
           let (neg5, rbytes) = case.get("r").unwrap();
           let (neg6, bbytes) = case.get("b").unwrap();
           let (neg7, tbytes) = case.get("t").unwrap();
         });
       }
     |]

byteShiftInfo :: Word -> (Word, Word)
byteShiftInfo idx =
    (idx `div` 8, (idx `mod` 8) * 8)

pad :: Int -> Char -> String -> String
pad len c str
  | length str >= len = str
  | otherwise         = pad len c (c:str)

generateZeroTests :: Word -> Word -> [Stmt Span]
generateZeroTests i max
  | i == max  = []
  | otherwise =
      let ilit = toLit i
      in [stmt| result = self.values[$$(ilit)] == 0; |] :
         generateZeroTests (i + 1) max

generatePackerLines :: Word -> Word -> [Stmt Span]
generatePackerLines i max
  | i == max = []
  | otherwise =
      let ilit = toLit i
          nextLit = toLit (i + 1)
          validx = toLit (i `div` 8)
          shiftx = toLit ((i `mod` 8) * 8)
          writeLine = [stmt| bytes[$$(ilit)] = (self.values[$$(validx)] >> $$(shiftx)) as u8; |]
          ifLine = [stmt| if bytes.len() == $$(nextLit) { return; } |]
      in writeLine : ifLine : generatePackerLines (i + 1) max

