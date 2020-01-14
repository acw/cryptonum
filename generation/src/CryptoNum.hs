{-# LANGUAGE QuasiQuotes #-}
module CryptoNum(
    cryptoNum
  )
 where

import Data.Bits(testBit)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import File
import Gen
import Generators
import Language.Rust.Data.Ident
import Language.Rust.Data.Position
import Language.Rust.Quote
import Language.Rust.Syntax
import System.Random(RandomGen)

numTestCases :: Int
numTestCases = 3000

cryptoNum :: File
cryptoNum = File {
  predicate = \ _ _ -> True,
  outputName = "cryptonum",
  isUnsigned = True,
  generator = declareCryptoNumInstance,
  testCase = Just generateTests
}

declareCryptoNumInstance :: Word -> [Word] -> SourceFile Span
declareCryptoNumInstance bitsize _ =
  let sname = mkIdent ("U" ++ show bitsize)
      entries = bitsize `div` 64
      entlit = Lit [] (Int Dec (fromIntegral entries) Unsuffixed mempty) mempty
      zeroTests = generateZeroTests 0 entries
      bitlength = toLit bitsize
      bytelen = bitsize `div` 8
      bytelenlit = toLit bytelen
      bytebuffer = Delimited mempty Bracket (Stream [
                     Tree (Token mempty (LiteralTok (IntegerTok "0") Nothing)),
                     Tree (Token mempty Semicolon),
                     Tree (Token mempty (LiteralTok (IntegerTok (show bytelen)) Nothing))
                   ])
      entrieslit = toLit entries
      testFileLit = Lit [] (Str (testFile True bitsize) Cooked Unsuffixed mempty) mempty
  in [sourceFile|
       use core::cmp::min;
       #[cfg(test)]
       use core::convert::TryFrom;
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
         fn is_odd(&self) -> bool {
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

            for x in bytes.iter_mut().take($$(bytelenlit)).rev() {
               *x = (self.value[idx] >> shift) as u8;
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
         run_test(build_test_path("cryptonum", $$(testFileLit)), 8, |case| {
           let (neg0, xbytes) = case.get("x").unwrap();
           let (neg1, mbytes) = case.get("m").unwrap();
           let (neg2, zbytes) = case.get("z").unwrap();
           let (neg3, ebytes) = case.get("e").unwrap();
           let (neg4, obytes) = case.get("o").unwrap();
           let (neg5, rbytes) = case.get("r").unwrap();
           let (neg6, bbytes) = case.get("b").unwrap();
           let (neg7, tbytes) = case.get("t").unwrap();

           assert!(!neg0 && !neg1 && !neg2 && !neg3 &&
                   !neg4 && !neg5 && !neg6 && !neg7);
           let mut x = $$sname::from_bytes(&xbytes);
           let z = 1 == zbytes[0];
           let e = 1 == ebytes[0];
           let o = 1 == obytes[0];
           let t = 1 == tbytes[0];
           let m = usize::try_from($$sname::from_bytes(&mbytes)).unwrap();
           let b = usize::try_from($$sname::from_bytes(&bbytes)).unwrap();
           let r = $$sname::from_bytes(&rbytes);
           assert_eq!(x.is_zero(), z);
           assert_eq!(x.is_even(), e);
           assert_eq!(x.is_odd(),  o);
           assert_eq!(x.testbit(b), t);
           x.mask(m);
           assert_eq!(x, r);
         });
       }
     |]

generateZeroTests :: Word -> Word -> [Stmt Span]
generateZeroTests i entries
  | i == entries  = []
  | otherwise =
      let ilit = toLit i
      in [stmt| result &= self.value[$$(ilit)] == 0; |] :
         generateZeroTests (i + 1) entries

generateTests :: RandomGen g => Word -> g -> [Map String String]
generateTests size g = go g numTestCases
 where
  go _  0 = []
  go g0 i =
   let (x, g1) = generateNum g0 size
       (m, g2) = generateNum g1 size
       (b, g3) = generateNum g2 16
       m'      = m `mod` (fromIntegral size `div` 64)
       r       = x `mod` (2 ^ (64 * m'))
       t       = x `testBit` (fromIntegral b)
       tcase   = Map.fromList [("x", showX x),        ("z", showB (x == 0)),
                               ("e", showB (even x)), ("o", showB (odd x)),
                               ("m", showX m'),       ("r", showX r),
                               ("b", showX b),        ("t", showB t)]
   in tcase : go g3 (i - 1)

