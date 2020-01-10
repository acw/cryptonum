{-# LANGUAGE QuasiQuotes #-}
module ModOps(modulusOps)
 where

import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import File
import Generators
import GHC.Integer.GMP.Internals(powModInteger)
import Language.Rust.Data.Ident
import Language.Rust.Data.Position
import Language.Rust.Quote
import Language.Rust.Syntax
import System.Random(RandomGen)

numTestCases :: Int
numTestCases = 1000

modulusOps :: File
modulusOps = File {
    predicate = \ me others -> (me * 2) `elem` others,
    outputName = "modops",
    isUnsigned = True,
    generator = declareModOps,
    testCase = Just generateModulusTests
}

declareModOps :: Word -> [Word] -> SourceFile Span
declareModOps bitsize _ =
    let sname = mkIdent ("U" ++ show bitsize)
        bname = mkIdent ("U" ++ show (bitsize * 2))
        testFileLit = Lit [] (Str (testFile bitsize) Cooked Unsuffixed mempty) mempty
    in [sourceFile|
        use core::convert::TryFrom; 
        use crate::unsigned::{$$sname, $$bname};
        use crate::{DivMod, ModularOperations};
        #[cfg(test)]
        use crate::CryptoNum;
        #[cfg(test)]
        use crate::testing::{build_test_path,run_test};

        impl ModularOperations for $$sname {
            fn reduce(&self, m: &$$sname) -> $$sname {
                let (_, res) = self.divmod(m);
                res
            }

            fn modmul(&self, y: &$$sname, m: &$$sname) -> $$sname {
                let r = self * y;
                let bigm = $$bname::from(m);
                let bigres = r % bigm;
                $$sname::try_from(bigres)
                        .expect("Mathematics is broken?! (mod returned too big result")
            }

            fn modsq(&self, m: &$$sname) -> $$sname {
                let r = self * self;
                let bigm = $$bname::from(m);
                let bigres = r % bigm;
                $$sname::try_from(bigres)
                        .expect("Mathematics is broken?! (mod returned too big result")
            }

            fn modexp(&self, e: &$$sname, m: &$$sname) -> $$sname {
                let mut r    = $$sname::from(1u64);
                let     bigm = $$bname::from(m);

                for digit in e.value.iter().rev() {
                    for bit in (0..64).rev() {
                        r = r.modsq(&m);
                        let big_possible_r = (&r * self) % &bigm;
                        let possible_r = $$sname::try_from(big_possible_r)
                                                 .expect("Math is broken (again)");
                        let bit = (*digit >> bit) & 1;
                        r = if bit == 1 { possible_r } else { r };
                    }
                }

                r
            }
        }

        #[cfg(test)]
        #[allow(non_snake_case)]
        #[test]
        fn KATs() {
            run_test(build_test_path("modops", $$(testFileLit)), 7, |case| {
                let (neg0, xbytes) = case.get("x").unwrap();
                let (neg1, ybytes) = case.get("y").unwrap();
                let (neg2, mbytes) = case.get("m").unwrap();
                let (neg3, rbytes) = case.get("r").unwrap();
                let (neg4, tbytes) = case.get("t").unwrap();
                let (neg5, sbytes) = case.get("s").unwrap();
                let (neg6, ebytes) = case.get("e").unwrap();

                assert!(!neg0 && !neg1 && !neg2 && !neg3 && !neg4 && !neg5 && !neg6);
                let x = $$sname::from_bytes(&xbytes);
                let y = $$sname::from_bytes(&ybytes);
                let m = $$sname::from_bytes(&mbytes);
                let r = $$sname::from_bytes(&rbytes);
                let t = $$sname::from_bytes(&tbytes);
                let s = $$sname::from_bytes(&sbytes);
                let e = $$sname::from_bytes(&ebytes);

                assert_eq!(r, x.reduce(&m));
                assert_eq!(t, x.modmul(&y, &m));
                assert_eq!(s, x.modsq(&m));
                assert_eq!(e, x.modexp(&y, &m));
            });
        }
     |]

generateModulusTests :: RandomGen g => Word -> g -> [Map String String]
generateModulusTests size g = go g numTestCases
 where
  go _  0 = []
  go g0 i =
    let (x, g1) = generateNum g0 size
        (y, g2) = generateNum g1 size
        (m, g3) = generateNum g2 size
        tcase   = Map.fromList [("x", showX x), ("y", showX y),
                                ("m", showX m),
                                ("r", showX (x `mod` m)),
                                ("t", showX ((x * y) `mod` m)),
                                ("s", showX ((x * x) `mod` m)),
                                ("e", showX (powModInteger x y m))
                               ]
    in if y < 2
         then go g3 i
         else tcase : go g3 (i - 1)


