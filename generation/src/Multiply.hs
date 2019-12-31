{-# LANGUAGE QuasiQuotes #-}
module Multiply(
    safeMultiplyOps
  , unsafeMultiplyOps
  )
 where

import Data.Bits((.&.))
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import File
import Gen(toLit)
import Generators
import Karatsuba
import Language.Rust.Data.Ident
import Language.Rust.Data.Position
import Language.Rust.Quote
import Language.Rust.Syntax
import System.Random(RandomGen)

numTestCases :: Int
numTestCases = 3000

safeMultiplyOps :: File
safeMultiplyOps = File {
    predicate = \ me others -> (me * 2) `elem` others,
    outputName = "safe_mul",
    isUnsigned = True,
    generator = declareSafeMulOperators,
    testCase = Just generateSafeTests
}

unsafeMultiplyOps :: File
unsafeMultiplyOps = File {
    predicate = \ _ _ -> False,
    outputName = "unsafe_mul",
    isUnsigned = True,
    generator = declareUnsafeMulOperators,
    testCase = Just generateUnsafeTests
}

declareSafeMulOperators :: Word -> SourceFile Span
declareSafeMulOperators bitsize =
  let sname = mkIdent ("U" ++ show bitsize)
      dname = mkIdent ("U" ++ show (bitsize * 2))
      fullRippleMul = undefined True (bitsize `div` 64) "res"
      testFileLit = Lit [] (Str (testFile bitsize) Cooked Unsuffixed mempty) mempty
  in [sourceFile|
        use core::ops::Mul;
        use crate::CryptoNum;
        #[cfg(test)]
        use crate::testing::{build_test_path,run_test};
        #[cfg(test)]
        use quickcheck::quickcheck;
        use crate::unsigned::{$$sname,$$dname};

        impl Mul for $$sname {
          type Output = $$dname;

          fn mul(self, rhs: $$sname) -> $$dname {
             &self + &rhs
          }
        }

        impl<'a> Mul<&'a $$sname> for $$sname {
          type Output = $$dname;

          fn mul(self, rhs: &$$sname) -> $$dname {
            &self + rhs
          }
        }

        impl<'a> Mul<$$sname> for &'a $$sname {
          type Output = $$dname;

          fn mul(self, rhs: $$sname) -> $$dname {
            self + &rhs
          }
        }

        impl<'a,'b> Mul<&'a $$sname> for &'b $$sname {
          type Output = $$dname;

          fn mul(self, rhs: &$$sname) -> $$dname {
            let mut res = $$dname::zero();

            $@{fullRippleMul}

            res
          }
        }

        #[cfg(test)]
        quickcheck! {
           fn multiplication_symmetric(a: $$sname, b: $$sname) -> bool {
             (&a * &b) == (&b * &a)
           }
        }

       #[cfg(test)]
       #[allow(non_snake_case)]
       #[test]
       fn KATs() {
         run_test(build_test_path("safe_mul", $$(testFileLit)), 3, |case| {
           let (neg0, xbytes) = case.get("x").unwrap();
           let (neg1, ybytes) = case.get("y").unwrap();
           let (neg2, zbytes) = case.get("z").unwrap();

           assert!(!neg0 && !neg1 && !neg2);
           let x = $$sname::from_bytes(&xbytes);
           let y = $$sname::from_bytes(&ybytes);
           let z = $$dname::from_bytes(&zbytes);

           assert_eq!(z, x + y);
        });
      }
      |]

declareUnsafeMulOperators :: Word -> SourceFile Span
declareUnsafeMulOperators bitsize = undefined bitsize

-- -----------------------------------------------------------------------------

translateInstruction :: Instruction -> Stmt Span
translateInstruction instr =
  case instr of
    Add        outname args ->
      let outid = mkIdent outname
          args' = map (\x -> [expr| $$x |]) (map mkIdent args)
          adds  = foldl (\ x y -> [expr| $$(x) + $$(y) |])
                        (head args')
                        (tail args')
      in [stmt| let $$outid: u128 = $$(adds); |]
    CastDown   outname arg  ->
      let outid = mkIdent outname
          inid  = mkIdent arg
      in [stmt| let $$outid: u64 = $$inid as u64; |]
    CastUp     outname arg  ->
      let outid = mkIdent outname
          inid  = mkIdent arg
      in [stmt| let $$outid: u128 = $$inid as u128; |]
    Complement outname arg  ->
      let outid = mkIdent outname
          inid  = mkIdent arg
      in [stmt| let $$outid: u64 = !$$inid; |]
    Declare64  outname arg  ->
      let outid = mkIdent outname
          val   = toLit (fromIntegral arg)
      in [stmt| let $$outid: u64 = $$(val); |]
    Declare128 outname arg  ->
      let outid = mkIdent outname
          val   = toLit (fromIntegral arg)
      in [stmt| let $$outid: u128 = $$(val); |]
    Mask       outname arg mask ->
      let outid = mkIdent outname
          inid  = mkIdent arg
          val   = toLit (fromIntegral mask)
      in [stmt| let $$outid: u128 = $$inid & $$(val); |]
    Multiply   outname args ->
      let outid = mkIdent outname
          args' = map (\x -> [expr| $$x |]) (map mkIdent args)
          muls  = foldl (\ x y -> [expr| $$(x) * $$(y) |])
                        (head args')
                        (tail args')
      in [stmt| let $$outid: u128 = $$(muls); |]
    ShiftR     outname arg amt ->
      let outid = mkIdent outname
          inid  = mkIdent arg
          val   = toLit (fromIntegral amt)
      in [stmt| let $$outid: u128 = $$inid >> $$(val); |]

-- -----------------------------------------------------------------------------

generateSafeTests :: RandomGen g => Word -> g -> [Map String String]
generateSafeTests size g = go g numTestCases
 where
  go _  0 = []
  go g0 i =
    let (x, g1) = generateNum g0 size
        (y, g2) = generateNum g1 size
        tcase   = Map.fromList [("x", showX x), ("y", showX y),
                                ("z", showX (x * y))]
    in tcase : go g2 (i - 1)

generateUnsafeTests :: RandomGen g => Word -> g -> [Map String String]
generateUnsafeTests size g = go g numTestCases
 where
  go _  0 = []
  go g0 i =
    let (x, g1) = generateNum g0 size
        (y, g2) = generateNum g1 size
        z       = (x * y) .&. ((2 ^ size) - 1)
        tcase   = Map.fromList [("x", showX x), ("y", showX y),
                                ("z", showX z)]
    in tcase : go g2 (i - 1)
