{-# LANGUAGE QuasiQuotes #-}
module Division(divisionOps)
 where

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

divisionOps :: File
divisionOps = File {
    predicate = \ _ _ -> True,
    outputName = "divmod",
    isUnsigned = True,
    generator = declareDivision,
    testCase = Just generateDivisionTests
}

declareDivision :: Word -> SourceFile Span
declareDivision size =
    let sname = mkIdent ("U" ++ show size)
        entries = size `div` 64
        copyAssign = map doCopy [0..entries-1]
        testFileLit = Lit [] (Str (testFile size) Cooked Unsuffixed mempty) mempty
    in [sourceFile|
        use core::ops::{Div, DivAssign};
        use core::ops::{Rem, RemAssign};
        use crate::CryptoNum;
        #[cfg(test)]
        use crate::testing::{build_test_path,run_test};
        use crate::unsigned::$$sname;
        use super::super::super::DivMod;

        impl DivMod for $$sname {
            fn divmod(&self, rhs: &$$sname) -> ($$sname, $$sname) {
                let mut q = $$sname::zero();
                let mut r = $$sname::zero();

                for (ndigit, qdigit) in self.value.iter().rev().zip(q.value.iter_mut().rev()) {
                    for i in (0..64).rev() {
                        let mut r1: $$sname = &r << 1u64;
                        r1.value[0] |= (ndigit >> i) & 1u64;
                        let mut r2: $$sname = r1.clone();
                        r2 -= rhs;
                        let (newr, bit) = if &r1 > rhs {
                            (r2, 1)
                        } else {
                            (r1, 0)
                        };
                        r = newr;
                        *qdigit |= bit << i;
                    }
                }

                (q, r)
            }
        }

        impl Div for $$sname {
            type Output = $$sname;

            fn div(self, rhs: $$sname) -> Self::Output {
                let (res, _) = self.divmod(&rhs);
                res
            }
        }

        impl<'a> Div<$$sname> for &'a $$sname {
            type Output = $$sname;

            fn div(self, rhs: $$sname) -> Self::Output {
                let (res, _) = self.divmod(&rhs);
                res
            }
        }

        impl<'a> Div<&'a $$sname> for $$sname {
            type Output = $$sname;

            fn div(self, rhs: &$$sname) -> Self::Output {
                let (res, _) = self.divmod(rhs);
                res
            }
        }

        impl<'a,'b> Div<&'a $$sname> for &'b $$sname {
            type Output = $$sname;

            fn div(self, rhs: &$$sname) -> Self::Output {
                let (res, _) = self.divmod(rhs);
                res
            }
        }

        impl DivAssign for $$sname {
            fn div_assign(&mut self, rhs: $$sname) {
                let (res, _) = self.divmod(&rhs);
                $@{copyAssign}
            }
        }

        impl<'a> DivAssign<&'a $$sname> for $$sname {
            fn div_assign(&mut self, rhs: &$$sname) {
                let (res, _) = self.divmod(rhs);
                $@{copyAssign}
            }
        }

        impl Rem for $$sname {
            type Output = $$sname;

            fn rem(self, rhs: $$sname) -> Self::Output {
                let (_, res) = self.divmod(&rhs);
                res
            }
        }

        impl<'a> Rem<$$sname> for &'a $$sname {
            type Output = $$sname;

            fn rem(self, rhs: $$sname) -> Self::Output {
                let (_, res) = self.divmod(&rhs);
                res
            }
        }

        impl<'a> Rem<&'a $$sname> for $$sname {
            type Output = $$sname;

            fn rem(self, rhs: &$$sname) -> Self::Output {
                let (_, res) = self.divmod(rhs);
                res
            }
        }

        impl<'a,'b> Rem<&'a $$sname> for &'b $$sname {
            type Output = $$sname;

            fn rem(self, rhs: &$$sname) -> Self::Output {
                let (_, res) = self.divmod(rhs);
                res
            }
        }

        impl RemAssign for $$sname {
            fn rem_assign(&mut self, rhs: $$sname) {
                let (_, res) = self.divmod(&rhs);
                $@{copyAssign}
            }
        }

        impl<'a> RemAssign<&'a $$sname> for $$sname {
            fn rem_assign(&mut self, rhs: &$$sname) {
                let (_, res) = self.divmod(rhs);
                $@{copyAssign}
            }
        }

       #[cfg(test)]
       #[allow(non_snake_case)]
       #[test]
       fn KATs() {
         run_test(build_test_path("divmod", $$(testFileLit)), 4, |case| {
           let (neg0, xbytes) = case.get("x").unwrap();
           let (neg1, ybytes) = case.get("y").unwrap();
           let (neg2, zbytes) = case.get("z").unwrap();
           let (neg3, rbytes) = case.get("r").unwrap();

           assert!(!neg0 && !neg1 && !neg2 && !neg3);
           let x = $$sname::from_bytes(&xbytes);
           let y = $$sname::from_bytes(&ybytes);
           let z = $$sname::from_bytes(&zbytes);
           let r = $$sname::from_bytes(&rbytes);

           let (myz, myr) = x.divmod(&y);

           assert_eq!(z, myz);
           assert_eq!(r, myr);
           assert_eq!(z, &x / &y);
           assert_eq!(r, &x % &y);
        });
       }
    |]

doCopy :: Word -> Stmt Span
doCopy i =
    let liti = toLit i
    in [stmt| self.value[$$(liti)] = res.value[$$(liti)]; |]

generateDivisionTests :: RandomGen g => Word -> g -> [Map String String]
generateDivisionTests size g = go g numTestCases
 where
  go _  0 = []
  go g0 i =
    let (x, g1) = generateNum g0 size
        (y, g2) = generateNum g1 size
        tcase   = Map.fromList [("x", showX x), ("y", showX y),
                                ("z", showX (x `div` y)),
                                ("r", showX (x `mod` y))]
    in if y == 0
         then go g0 i
         else tcase : go g2 (i - 1)

