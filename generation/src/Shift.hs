{-# LANGUAGE QuasiQuotes #-}
module Shift(shiftOps, signedShiftOps)
 where

import Data.Bits(shiftL,shiftR)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import Generators
import Language.Rust.Data.Ident
import Language.Rust.Data.Position
import Language.Rust.Quote
import Language.Rust.Syntax
import RustModule
import System.Random(RandomGen)

shiftOps :: RustModule
shiftOps = RustModule {
    predicate = \ _ _ -> True,
    suggested = const [],
    outputName = "shift",
    isUnsigned = True,
    generator = declareShiftOperators,
    testCase = Just generateTest
}

signedShiftOps :: RustModule
signedShiftOps = RustModule {
    predicate = \ _ _ -> True,
    suggested = const [],
    outputName = "sshift",
    isUnsigned = False,
    generator = declareSignedShiftOperators,
    testCase = Just generateSignedTest
}

declareShiftOperators :: Word -> [Word] -> SourceFile Span
declareShiftOperators bitsize _ =
  let struct_name = mkIdent ("U" ++ show bitsize)
      entries = bitsize `div` 64
      unsignedShifts = generateUnsigneds struct_name
      shlUsizeImpls = generateBaseUsizes struct_name
      shlActualImpl = concatMap actualShlImplLines [1..entries-1]
      shrActualImpl = concatMap (actualShrImplLines False entries) (reverse [0..entries-1])
      resAssign = map (reassignSelf False) [0..entries-1]
      testFileLit = Lit [] (Str (testFile True bitsize) Cooked Unsuffixed mempty) mempty
  in [sourceFile|
       #[cfg(test)]
       use core::convert::TryFrom;
       use core::ops::{Shl,ShlAssign};
       use core::ops::{Shr,ShrAssign};
       #[cfg(test)]
       use crate::CryptoNum;
       #[cfg(test)]
       use crate::testing::{build_test_path,run_test};
       use super::$$struct_name;

       impl ShlAssign<usize> for $$struct_name {
         fn shl_assign(&mut self, rhs: usize) {
           let digits  = rhs / 64;
           let bits    = rhs % 64;
           let shift   = (64 - bits) as u32;

           let base0 = if digits == 0 { self.value[0] } else { 0 };
           let res0  = base0 << bits;
           $@{shlActualImpl}
           $@{resAssign}
         }
       }

       impl ShrAssign<usize> for $$struct_name {
         fn shr_assign(&mut self, rhs: usize) {
           let digits     = rhs / 64;
           let bits       = rhs % 64;
           let mask       = !(0xFFFFFFFFFFFFFFFFu64 << bits);
           let shift      = (64 - bits) as u32;
           let arith_base = 0;

           $@{shrActualImpl}
           $@{resAssign}
         }
       }

       $@{shlUsizeImpls}
       $@{unsignedShifts}

       #[cfg(test)]
       #[allow(non_snake_case)]
       #[test]
       fn KATs() {
         run_test(build_test_path("shift", $$(testFileLit)), 4, |case| {
           let (neg0, xbytes) = case.get("x").unwrap();
           let (neg1, sbytes) = case.get("s").unwrap();
           let (neg2, lbytes) = case.get("l").unwrap();
           let (neg3, rbytes) = case.get("r").unwrap();

           assert!(!neg0 && !neg1 && !neg2 && !neg3);
           let x = $$struct_name::from_bytes(xbytes);
           let s = usize::try_from($$struct_name::from_bytes(sbytes)).unwrap();
           let l = $$struct_name::from_bytes(lbytes);
           let r = $$struct_name::from_bytes(rbytes);

           assert_eq!(l, &x << s);
           assert_eq!(r, &x >> s);
         });
       }
     |]

declareSignedShiftOperators :: Word -> [Word] -> SourceFile Span
declareSignedShiftOperators bitsize _ =
  let struct_name = mkIdent ("I" ++ show bitsize)
      entries = bitsize `div` 64
      unsignedShifts = generateUnsigneds struct_name
      shlUsizeImpls = generateBaseUsizes struct_name
      shrActualImpl = concatMap (actualShrImplLines True entries) (reverse [0..entries-1])
      resAssign = map (reassignSelf True) [0..entries-1]
      testFileLit = Lit [] (Str (testFile False bitsize) Cooked Unsuffixed mempty) mempty
  in [sourceFile|
       #[cfg(test)]
       use core::convert::TryFrom;
       use core::ops::{Shl,ShlAssign};
       use core::ops::{Shr,ShrAssign};
       #[cfg(test)]
       use crate::CryptoNum;
       #[cfg(test)]
       use crate::testing::{build_test_path,run_test};
       use super::$$struct_name;

       impl ShlAssign<usize> for $$struct_name {
         fn shl_assign(&mut self, rhs: usize) {
           self.contents <<= rhs;
         }
       }

       impl ShrAssign<usize> for $$struct_name {
         fn shr_assign(&mut self, rhs: usize) {
           let digits     = rhs / 64;
           let bits       = rhs % 64;
           let mask       = !(0xFFFFFFFFFFFFFFFFu64 << bits);
           let shift      = (64 - bits) as u32;
           let arith_base = if self.is_negative() {
              0xFFFF_FFFF_FFFF_FFFFu64
           } else {
              0
           };

           $@{shrActualImpl}
           $@{resAssign}
         }
       }

       $@{shlUsizeImpls}
       $@{unsignedShifts}

       #[cfg(test)]
       #[allow(non_snake_case)]
       #[test]
       fn KATs() {
         run_test(build_test_path("sshift", $$(testFileLit)), 4, |case| {
           let (neg0, xbytes) = case.get("x").unwrap();
           let (neg1, sbytes) = case.get("s").unwrap();
           let (neg2, lbytes) = case.get("l").unwrap();
           let (neg3, rbytes) = case.get("r").unwrap();

           assert!(!neg1);
           let mut x = $$struct_name::from_bytes(xbytes);
           let mut l = $$struct_name::from_bytes(lbytes);
           let mut r = $$struct_name::from_bytes(rbytes);

           if *neg0 { x = -x }
           if *neg2 { l = -l }
           if *neg3 { r = -r }
           let s = usize::try_from($$struct_name::from_bytes(sbytes)).unwrap();

           assert_eq!(l, &x << s);
           assert_eq!(r, &x >> s);
         });
       }
     |]

actualShlImplLines :: Word -> [Stmt Span]
actualShlImplLines i =
  let basei = mkIdent ("base" ++ show i)
      basei1 = mkIdent ("base" ++ show (i - 1))
      carryi = mkIdent ("carry" ++ show i)
      resi = mkIdent ("res" ++ show i)
      liti = toLit i
  in  [
    [stmt|let $$basei = if $$(liti) >= digits {
                          self.value[$$(liti)-digits]
                        } else {
                          0
                        }; |]
  , [stmt|let $$carryi = if shift == 64 { 0 } else { $$basei1 >> shift }; |]
  , [stmt|let $$resi = ($$basei << bits) | $$carryi; |]
  ]

actualShrImplLines :: Bool -> Word -> Word -> [Stmt Span]
actualShrImplLines prefix_field entries i =
  let basei = mkIdent ("base" ++ show i)
      carryi = mkIdent ("carry" ++ show i)
      carryi1 = mkIdent ("carry" ++ show (i + 1))
      targeti = mkIdent ("target" ++ show i)
      resi = mkIdent ("res" ++ show i)
      liti = toLit i
      litentries = toLit entries
      sourceI | prefix_field = [expr| self.contents.value[$$targeti] |]
              | otherwise    = [expr| self.value[$$targeti] |]
  in concat [
    [[stmt|let $$targeti = $$(liti) + digits; |]]
  , [[stmt|let $$basei = if $$targeti >= $$(litentries) { arith_base } else { $$(sourceI) }; |]]
  , if i == (entries - 1)
      then [[stmt| let ($$carryi1,_) = (arith_base & mask).overflowing_shl(shift); |]]
      else []
  , if i == 0
      then []
      else [[stmt|let ($$carryi,_) = ($$basei & mask).overflowing_shl(shift); |]]
  , [[stmt|let $$resi = ($$basei >> bits) | $$carryi1; |]]
  ]

reassignSelf :: Bool -> Word -> Stmt Span
reassignSelf prefix_field i =
  let liti = toLit i
      resi = mkIdent ("res" ++ show i)
  in if prefix_field
       then [stmt| self.contents.value[$$(liti)] = $$resi; |]
       else [stmt| self.value[$$(liti)]          = $$resi; |]

generateBaseUsizes :: Ident -> [Item Span]
generateBaseUsizes sname =
  generateBaseUsize sname (mkIdent "Shl") (mkIdent "shl") (mkIdent "shl_assign") ++
  generateBaseUsize sname (mkIdent "Shr") (mkIdent "shr") (mkIdent "shr_assign")

generateBaseUsize :: Ident -> Ident -> Ident -> Ident -> [Item Span]
generateBaseUsize sname tname sfn assign = [
    [item|
       impl $$tname<usize> for $$sname {
         type Output = Self;

         fn $$sfn(mut self, rhs: usize) -> $$sname {
           self.$$assign(rhs);
           self
         }
       }
    |]
  , [item|
       impl<'a> $$tname<usize> for &'a $$sname {
         type Output = $$sname;

         fn $$sfn(self, rhs: usize) -> $$sname {
           let mut res = self.clone();
           res.$$assign(rhs);
           res
         }
       }
    |]
  ]


generateUnsigneds :: Ident -> [Item Span]
generateUnsigneds sname =
  concatMap (generateUnsignedImpls sname . mkIdent) ["u8","u16","u32","u64","u128"]

generateUnsignedImpls :: Ident -> Ident -> [Item Span]
generateUnsignedImpls sname rhs =
  generateBaseImpls sname (mkIdent "Shl")       (mkIdent "shl")
                          (mkIdent "ShlAssign") (mkIdent "shl_assign") rhs ++
  generateBaseImpls sname (mkIdent "Shr")       (mkIdent "shr")
                          (mkIdent "ShrAssign") (mkIdent "shr_assign") rhs

generateBaseImpls :: Ident -> Ident -> Ident -> Ident -> Ident -> Ident -> [Item Span]
generateBaseImpls sname upper_shift lower_shift assign_shift lassign_shift right = [
    [item|
      impl $$assign_shift<$$right> for $$sname {
        fn $$lassign_shift(&mut self, rhs: $$right) {
          self.$$lassign_shift(rhs as usize);
        }
      }
    |]
  , [item|
      impl $$upper_shift<$$right> for $$sname {
        type Output = $$sname;

        fn $$lower_shift(self, rhs: $$right) -> Self::Output {
          self.$$lower_shift(rhs as usize)
        }
      }
    |]
  , [item|
      impl<'a> $$upper_shift<$$right> for &'a $$sname {
        type Output = $$sname;

        fn $$lower_shift(self, rhs: $$right) -> $$sname {
          self.$$lower_shift(rhs as usize)
        }
      }
    |]
  ]

generateTest :: RandomGen g => Word -> g -> (Map String String, g)
generateTest size g0 = (tcase, g2)
 where
  (x, g1) = generateNum g0 size
  (y, g2) = generateNum g1 size
  s       = y `mod` fromIntegral size
  l       = modulate (x `shiftL` fromIntegral s) size
  r       = modulate (x `shiftR` fromIntegral s) size
  tcase   = Map.fromList [("x", showX x), ("s", showX s),
                          ("l", showX l), ("r", showX r)]

generateSignedTest :: RandomGen g => Word -> g -> (Map String String, g)
generateSignedTest size g0 = (tcase, g2)
 where
  (x, g1) = generateSignedNum g0 size
  (y, g2) = generateNum g1 size
  s       = y `mod` fromIntegral size
  l       = modulate (x `shiftL` fromIntegral s) size
  r       = modulate (x `shiftR` fromIntegral s) size
  tcase   = Map.fromList [("x", showX x), ("s", showX s),
                          ("l", showX l), ("r", showX r)]