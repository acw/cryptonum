{-# LANGUAGE QuasiQuotes #-}
module ModOps(modulusOps)
 where

import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import File
import Gen(toLit)
import Generators
import GHC.Integer.GMP.Internals(powModInteger)
import Language.Rust.Data.Ident
import Language.Rust.Data.Position
import Language.Rust.Quote
import Language.Rust.Syntax
import System.Random(RandomGen)

numTestCases :: Int
numTestCases = 3000

modulusOps :: File
modulusOps = File {
    predicate = \ me others -> (me * 2) `elem` others,
    outputName = "modops",
    isUnsigned = True,
    generator = declareModOps,
    testCase = Just generateModulusTests
}

declareModOps :: Word -> SourceFile Span
declareModOps bitsize =
    let sname = mkIdent ("U" ++ show bitsize)
        bname = mkIdent ("U" ++ show (bitsize * 2))
    in [sourceFile| 
        use crate::unsigned::$$sname;
        use crate::{DivMod, ModularOperations};

        impl ModularOperations for $$sname {
            fn reduce(&self, m: &$$sname) -> $$sname {
                let (_, res) = self.divmod(m);
                res
            }

            fn modmul(&self, y: &$$sname, m: &$$sname) -> $$sname {
                panic!("modmul")
            }

            fn modsq(&self, m: &$$sname) -> $$sname {
                panic!("reduce")
            }

            fn modexp(&self, e: &$$sname, m: &$$sname) -> $$sname {
                panic!("reduce")
            }
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


