{-# LANGUAGE QuasiQuotes #-}
module Multiply(
    safeMultiplyOps
  , unsafeMultiplyOps
  )
 where

import Data.Bits((.&.))
import Data.List(foldl')
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Generators
import Karatsuba
import Language.Rust.Data.Ident
import Language.Rust.Data.Position
import Language.Rust.Quote
import Language.Rust.Syntax
import RustModule
import System.Random(RandomGen)

safeMultiplyOps :: RustModule
safeMultiplyOps = RustModule {
    predicate = \ me others -> (me * 2) `elem` others,
    suggested = \ me -> [me * 2],
    outputName = "safe_mul",
    isUnsigned = True,
    generator = declareSafeMulOperators,
    testCase = Just generateSafeTest
}

unsafeMultiplyOps :: RustModule
unsafeMultiplyOps = RustModule {
    predicate = \ _ _ -> True,
    suggested = const [],
    outputName = "unsafe_mul",
    isUnsigned = True,
    generator = declareUnsafeMulOperators,
    testCase = Just generateUnsafeTest
}

declareSafeMulOperators :: Word -> [Word] -> SourceFile Span
declareSafeMulOperators bitsize _ =
  let sname = mkIdent ("U" ++ show bitsize)
      dname = mkIdent ("U" ++ show (bitsize * 2))
      fullRippleMul = generateMultiplier True (bitsize `div` 64) "rhs" "res"
      testFileLit = Lit [] (Str (testFile True bitsize) Cooked Unsuffixed mempty) mempty
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
             &self * &rhs
          }
        }

        impl<'a> Mul<&'a $$sname> for $$sname {
          type Output = $$dname;

          fn mul(self, rhs: &$$sname) -> $$dname {
            &self * rhs
          }
        }

        impl<'a> Mul<$$sname> for &'a $$sname {
          type Output = $$dname;

          fn mul(self, rhs: $$sname) -> $$dname {
            self * &rhs
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

           assert_eq!(z, x * y);
        });
      }
      |]

declareUnsafeMulOperators :: Word -> [Word] -> SourceFile Span
declareUnsafeMulOperators bitsize _ =
  let sname = mkIdent ("U" ++ show bitsize)
      halfRippleMul = generateMultiplier False (bitsize `div` 64) "rhs" "self"
      testFileLit = Lit [] (Str (testFile True bitsize) Cooked Unsuffixed mempty) mempty
  in [sourceFile|
        use core::ops::MulAssign;
        #[cfg(test)]
        use crate::CryptoNum;
        #[cfg(test)]
        use crate::testing::{build_test_path,run_test};
        #[cfg(test)]
        use quickcheck::quickcheck;
        use crate::unsigned::$$sname;

        impl MulAssign for $$sname {
          fn mul_assign(&mut self, rhs: $$sname) {
            self.mul_assign(&rhs);
          }
        }

        impl<'a> MulAssign<&'a $$sname> for $$sname {
          fn mul_assign(&mut self, rhs: &$$sname) {
            $@{halfRippleMul}
          }
        }

        #[cfg(test)]
        quickcheck! {
           fn multiplication_symmetric(a: $$sname, b: $$sname) -> bool {
             let     a2 = a.clone();
             let mut b2 = b.clone();
             let mut a3 = a.clone();
             let     b3 = b.clone();

             b2 *= &a2;
             a3 *= b3;

             a3 == b2
           }
        }

       #[cfg(test)]
       #[allow(non_snake_case)]
       #[test]
       fn KATs() {
         run_test(build_test_path("unsafe_mul", $$(testFileLit)), 3, |case| {
           let (neg0, xbytes) = case.get("x").unwrap();
           let (neg1, ybytes) = case.get("y").unwrap();
           let (neg2, zbytes) = case.get("z").unwrap();

           assert!(!neg0 && !neg1 && !neg2);
           let mut x = $$sname::from_bytes(&xbytes);
           let y = $$sname::from_bytes(&ybytes);
           let z = $$sname::from_bytes(&zbytes);

           x *= y;
           assert_eq!(z, x);
        });
      }
      |]


-- -----------------------------------------------------------------------------

generateMultiplier :: Bool -> Word -> String -> String -> [Stmt Span]
generateMultiplier fullmul size inName outName =
  let readIns = map (load "self" "x") [0..size-1] ++
                map (load inName "y") [0..size-1]
      instructions = releaseUnnecessary outVars (generateInstructions size)
      outDigits | fullmul   = 2 * size
                | otherwise = size
      outVars = map (("res" ++) . show) [0..outDigits-1]
      operations = map translateInstruction instructions
      writeOuts = map (store "res") [0..outDigits-1]
  in readIns ++ operations ++ writeOuts
 where
   load rhs vname i =
     let liti = toLit i
         vec = mkIdent rhs
         var = mkIdent (vname ++ show i)
     in [stmt| let $$var = $$vec.value[$$(liti)]; |]
   store vname i =
     let liti = toLit i
         vec = mkIdent outName
         var = mkIdent (vname ++ show i)
     in [stmt| $$vec.value[$$(liti)] = $$var; |]

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

releaseUnnecessary :: [String] -> [Instruction] -> [Instruction]
releaseUnnecessary outkeys instrs = go (Set.fromList outkeys) [] rInstrs
 where
   rInstrs = reverse instrs
   --
   go _ acc [] = acc
   go required acc (cur:rest)
     | outVar cur `Set.member` required =
         go (foldl' (flip Set.insert) required (inVars cur)) (cur:acc) rest
     | otherwise =
         go required acc rest

outVar :: Instruction -> String
outVar instr =
  case instr of
    Add        outname _   -> outname
    CastDown   outname _   -> outname
    CastUp     outname _   -> outname
    Complement outname _   -> outname
    Declare64  outname _   -> outname
    Declare128 outname _   -> outname
    Mask       outname _ _ -> outname
    Multiply   outname _   -> outname
    ShiftR     outname _ _ -> outname

inVars :: Instruction -> [String]
inVars instr =
  case instr of
    Add        _ args  -> args
    CastDown   _ arg   -> [arg]
    CastUp     _ arg   -> [arg]
    Complement _ arg   -> [arg]
    Declare64  _ _     -> []
    Declare128 _ _     -> []
    Mask       _ arg _ -> [arg]
    Multiply   _ args  -> args
    ShiftR     _ arg _ -> [arg]

-- -----------------------------------------------------------------------------

generateSafeTest :: RandomGen g => Word -> g -> (Map String String, g)
generateSafeTest size g0 = (tcase, g2)
 where
  (x, g1) = generateNum g0 size
  (y, g2) = generateNum g1 size
  tcase   = Map.fromList [("x", showX x), ("y", showX y), ("z", showX (x * y))]

generateUnsafeTest :: RandomGen g => Word -> g -> (Map String String, g)
generateUnsafeTest size g0 = (tcase, g2)
 where
  (x, g1) = generateNum g0 size
  (y, g2) = generateNum g1 size
  z       = (x * y) .&. ((2 ^ size) - 1)
  tcase   = Map.fromList [("x", showX x), ("y", showX y), ("z", showX z)]
