{-# LANGUAGE QuasiQuotes #-}
module BinaryOps(
    binaryOps
  )
 where

import Data.Bits(xor,(.&.),(.|.))
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import File
import Gen(toLit)
import Generators
import Language.Rust.Data.Ident
import Language.Rust.Data.Position
import Language.Rust.Quote
import Language.Rust.Syntax
import System.Random(RandomGen)

numTestCases :: Int
numTestCases = 3000

binaryOps :: File
binaryOps = File {
    predicate = \ _ _ -> True,
    outputName = "binary",
    isUnsigned = True,
    generator = declareBinaryOperators,
    testCase = Just generateTests
}

declareBinaryOperators :: Word -> [Word] -> SourceFile Span
declareBinaryOperators bitsize _ =
  let struct_name = mkIdent ("U" ++ show bitsize)
      entries = bitsize `div` 64
      andOps = generateBinOps "BitAnd" struct_name "bitand" BitAndOp entries
      orOps  = generateBinOps "BitOr"  struct_name "bitor"  BitOrOp  entries
      xorOps = generateBinOps "BitXor" struct_name "bitxor" BitXorOp entries
      baseNegationStmts = negationStatements "self" entries
      refNegationStmts = negationStatements "output" entries
      testFileLit = Lit [] (Str (testFile bitsize) Cooked Unsuffixed mempty) mempty
  in [sourceFile|
       use core::ops::{BitAnd,BitAndAssign};
       use core::ops::{BitOr,BitOrAssign};
       use core::ops::{BitXor,BitXorAssign};
       use core::ops::Not;
       #[cfg(test)]
       use crate::CryptoNum;
       #[cfg(test)]
       use crate::testing::{build_test_path,run_test};
       #[cfg(test)]
       use quickcheck::quickcheck;
       use super::$$struct_name;

       $@{andOps}
       $@{orOps}
       $@{xorOps}

       impl Not for $$struct_name {
         type Output = Self;

         fn not(mut self) -> Self {
            $@{baseNegationStmts}
            self
         }
       }

       impl<'a> Not for &'a $$struct_name {
         type Output = $$struct_name;

         fn not(self) -> Self::Output {
            let mut output = self.clone();
            $@{refNegationStmts}
            output
         }
       }

       #[cfg(test)]
       quickcheck! {
         fn and_associative(a: $$struct_name, b: $$struct_name, c: $$struct_name) -> bool {
           ((&a & &b) & &c) == (&a & (&b & &c))
         }
         fn and_commutative(a: $$struct_name, b: $$struct_name) -> bool {
           (&a & &b) == (&b & &a)
         }
         fn and_idempotent(a: $$struct_name, b: $$struct_name) -> bool {
           (&a & &b) == (&a & &b & &a)
         }

         fn xor_associative(a: $$struct_name, b: $$struct_name, c: $$struct_name) -> bool {
           ((&a ^ &b) ^ &c) == (&a ^ (&b ^ &c))
         }
         fn xor_commutative(a: $$struct_name, b: $$struct_name) -> bool {
           (&a ^ &b) == (&b ^ &a)
         }

         fn or_associative(a: $$struct_name, b: $$struct_name, c: $$struct_name) -> bool {
           ((&a | &b) | &c) == (&a | (&b | &c))
         }
         fn or_commutative(a: $$struct_name, b: $$struct_name) -> bool {
           (&a | &b) == (&b | &a)
         }
         fn or_idempotent(a: $$struct_name, b: $$struct_name) -> bool {
           (&a | &b) == (&a | &b | &a)
         }

         fn and_or_distribution(a: $$struct_name, b: $$struct_name, c: $$struct_name) -> bool {
           (&a & (&b | &c)) == ((&a & &b) | (&a & &c))
         }
         fn xor_clears(a: $$struct_name) -> bool {
           $$struct_name::zero() == (&a ^ &a)
         }
         fn double_neg_ident(a: $$struct_name) -> bool {
           a == !!&a
         }
         fn and_ident(a: $$struct_name) -> bool {
           let ones = !$$struct_name::zero();
           (&a & &ones) == a
         }
         fn or_ident(a: $$struct_name) -> bool {
           (&a | $$struct_name::zero()) == a
         }
       }

       #[cfg(test)]
       #[allow(non_snake_case)]
       #[test]
       fn KATs() {
         run_test(build_test_path("binary", $$(testFileLit)), 6, |case| {
           let (neg0, xbytes) = case.get("x").unwrap();
           let (neg1, ybytes) = case.get("y").unwrap();
           let (neg2, abytes) = case.get("a").unwrap();
           let (neg3, obytes) = case.get("o").unwrap();
           let (neg4, ebytes) = case.get("e").unwrap();
           let (neg5, nbytes) = case.get("n").unwrap();

           assert!(!neg0 && !neg1 && !neg2 && !neg3 && !neg4 && !neg5);
           let x = $$struct_name::from_bytes(&xbytes);
           let y = $$struct_name::from_bytes(&ybytes);
           let a = $$struct_name::from_bytes(&abytes);
           let o = $$struct_name::from_bytes(&obytes);
           let e = $$struct_name::from_bytes(&ebytes);
           let n = $$struct_name::from_bytes(&nbytes);

           assert_eq!(a, &x & &y);
           assert_eq!(o, &x | &y);
           assert_eq!(e, &x ^ &y);
           assert_eq!(n, !x);
        });
      }
     |]

negationStatements :: String -> Word -> [Stmt Span]
negationStatements target entries = map genStatement [0..entries-1]
 where
  genStatement i =
    let idx = toLit i
        v = mkIdent target
    in [stmt| $$v.value[$$(idx)] = !self.value[$$(idx)]; |]

generateBinOps :: String -> Ident -> String -> BinOp -> Word -> [Item Span]
generateBinOps trait sname func oper entries =
  [normAssign, refAssign] ++
  generateAllTheVariants traitIdent funcIdent sname oper entries
 where
  traitIdent = mkIdent trait
  assignIdent = mkIdent (trait ++ "Assign")
  funcIdent = mkIdent func
  funcAssignIdent = mkIdent (func ++ "_assign")
  --
  normAssign = [item|
    impl $$assignIdent for $$sname {
      fn $$funcAssignIdent(&mut self, rhs: Self) {
        $@{assignStatements}
      }
    }
  |]
  refAssign = [item|
    impl<'a> $$assignIdent<&'a $$sname> for $$sname {
      fn $$funcAssignIdent(&mut self, rhs: &Self) {
        $@{assignStatements}
      }
    }
  |]
  --
  assignStatements :: [Stmt Span]
  assignStatements = map genAssign [0..entries-1]
  genAssign i =
    let idx = toLit i
        left = [expr| self.value[$$(idx)] |]
        right = [expr| rhs.value[$$(idx)] |]
    in Semi (AssignOp [] oper left right mempty) mempty

generateAllTheVariants :: Ident -> Ident -> Ident -> BinOp -> Word -> [Item Span]
generateAllTheVariants traitname func sname oper entries = [
    [item|
     impl $$traitname for $$sname {
       type Output = $$sname;

       fn $$func(mut self, rhs: $$sname) -> Self::Output {
         $@{assigners_self_rhs}
         self
       }
     }|]
  , [item|
     impl<'a> $$traitname<&'a $$sname> for $$sname {
       type Output = $$sname;

       fn $$func(mut self, rhs: &$$sname) -> Self::Output {
         $@{assigners_self_rhs}
         self
       }
     }|]
  , [item|
     impl<'a> $$traitname<$$sname> for &'a $$sname {
       type Output = $$sname;

       fn $$func(self, mut rhs: $$sname) -> Self::Output {
         $@{assigners_rhs_self}
         rhs
       }
     }|]
  , [item|
     impl<'a,'b> $$traitname<&'a $$sname> for &'b $$sname {
       type Output = $$sname;

       fn $$func(self, rhs: &$$sname) -> Self::Output {
         let mut out = self.clone();
         $@{assigners_out_rhs}
         out
       }
     }|]
  ]
 where
  assigners_self_rhs = assigners [expr| self |] [expr| rhs  |]
  assigners_rhs_self = assigners [expr| rhs  |] [expr| self |]
  assigners_out_rhs  = assigners [expr| out  |] [expr| rhs  |]
  assigners left right = map (genAssign left right . toLit) [0..entries-1]
  genAssign left right i =
    Semi (AssignOp [] oper [expr| $$(left).value[$$(i)] |]
                           [expr| $$(right).value[$$(i)] |]
                           mempty) mempty

generateTests :: RandomGen g => Word -> g -> [Map String String]
generateTests size g = go g numTestCases
 where
  go _  0 = []
  go g0 i =
   let (x, g1) = generateNum g0 size
       (y, g2) = generateNum g1 size
       tcase   = Map.fromList [("x", showX x), ("y", showX y),
                               ("a", showX (x .&. y)),
                               ("o", showX (x .|. y)),
                               ("e", showX (x `xor` y)),
                               ("n", showX ( ((2 ^ size) - 1) `xor` x ))]
   in tcase : go g2 (i - 1)
