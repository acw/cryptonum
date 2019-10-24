{-# LANGUAGE QuasiQuotes #-}
module BinaryOps(
    binaryOps
  )
 where

import File
import Gen(toLit)
import Language.Rust.Data.Ident
import Language.Rust.Data.Position
import Language.Rust.Quote
import Language.Rust.Syntax

binaryOps :: File
binaryOps = File {
    predicate = \ _ _ -> True,
    outputName = "binary",
    generator = declareBinaryOperators
}

declareBinaryOperators :: Word -> SourceFile Span
declareBinaryOperators bitsize =
  let struct_name = mkIdent ("U" ++ show bitsize)
      entries = bitsize `div` 64
      andOps = generateBinOps "BitAnd" struct_name "bitand" BitAndOp entries
      orOps  = generateBinOps "BitOr"  struct_name "bitor"  BitOrOp  entries
      xorOps = generateBinOps "BitXor" struct_name "bitxor" BitXorOp entries
      baseNegationStmts = negationStatements "self" entries
      refNegationStmts = negationStatements "output" entries
  in [sourceFile|
       use core::ops::{BitAnd,BitAndAssign};
       use core::ops::{BitOr,BitOrAssign};
       use core::ops::{BitXor,BitXorAssign};
       use core::ops::Not;
       #[cfg(test)]
       use crate::CryptoNum;
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
           ((&a | &b) & &c) == (&a | (&b | &c))
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
