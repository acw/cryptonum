{-# LANGUAGE QuasiQuotes #-}
module Base(
    base
  )
 where

import File
import Language.Rust.Data.Ident
import Language.Rust.Data.Position
import Language.Rust.Quote
import Language.Rust.Syntax

base :: File
base = File {
  predicate = \ _ _ -> True,
  outputName = "base",
  generator = declareBaseStructure
}

declareBaseStructure :: Word -> SourceFile Span
declareBaseStructure bitsize =
  let tname = "U" ++ show bitsize
      entries = bitsize `div` 64
      sname = mkIdent tname
      entriese = Lit [] (Int Dec (fromIntegral entries) Unsuffixed mempty) mempty
      strname = Lit [] (Str tname Cooked Unsuffixed mempty) mempty
      debugExp = buildDebugExp 0 entries [expr| f.debug_tuple($$(strname)) |]
      lowerPrints = buildPrints entries "x"
      upperPrints = buildPrints entries "X"
  in [sourceFile|
        use core::fmt;
        use quickcheck::{Arbitrary,Gen};

        #[derive(Clone)]
        pub struct $$sname {
          pub(crate) value: [u64; $$(entriese)]
        }

        impl fmt::Debug for $$sname {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                $$(debugExp).finish()
            }
        }

        impl fmt::UpperHex for $$sname {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                $@{upperPrints}
                write!(f, "{:X}", self.value[0])
            }
        }

        impl fmt::LowerHex for $$sname {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                $@{lowerPrints}
                write!(f, "{:x}", self.value[0])
            }
        }

        impl Arbitrary for $$sname {
            fn arbitrary<G: Gen>(g: &mut G) -> Self {
                let mut res = $$sname{ value: [0; $$(entriese)] };
                for entry in res.value.iter_mut() {
                    *entry = g.next_u64();
                }
                res
            }
        }
     |]

buildDebugExp :: Word -> Word -> Expr Span -> Expr Span
buildDebugExp i top acc
  | i == top  = acc
  | otherwise =
     let liti = Lit [] (Int Dec (fromIntegral i) Unsuffixed mempty) mempty
     in buildDebugExp (i + 1) top [expr| $$(acc).field(&self.value[$$(liti)]) |]

buildPrints :: Word -> String -> [Stmt Span]
buildPrints entries printer = go (entries - 1)
 where
  litStr = Token mempty (LiteralTok (StrTok ("{:" ++ printer ++ "}")) Nothing)
  --Lit [] (Str ("{:" ++ printer ++ "}") Cooked Unsuffixed mempty) mempty
  go 0 = []
  go x =
    let rest = go (x - 1)
        curi = Token mempty (LiteralTok (IntegerTok (show x)) Nothing)
        -- Lit [] (Int Dec (fromIntegral x) Unsuffixed mempty) mempty
        cur = [stmt| write!(f, $$(litStr), self.value[$$(curi)])?; |]
    in cur : rest

--     implFor "fmt::UpperHex" name $
--       wrapIndent "fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result" $
--         do forM_ (reverse [1..top]) $ \ i ->
--              out ("write!(f, \"{:X}\", self.value[" ++ show i ++ "])?;")
--            out "write!(f, \"{:X}\", self.value[0])"
--     blank
--     implFor "fmt::LowerHex" name $
--       wrapIndent "fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result" $
--         do forM_ (reverse [1..top]) $ \ i ->
--              out ("write!(f, \"{:x}\", self.value[" ++ show i ++ "])?;")
--            out "write!(f, \"{:x}\", self.value[0])"
--     blank
--     implFor "Arbitrary" name $
--       wrapIndent "fn arbitrary<G: Gen>(g: &mut G) -> Self" $
--         do out (name ++ " {")
--            indent $
--              do out ("value: [")
--                 indent $ forM_ [0..top] $ \ _ ->
--                   out ("g.next_u64(),")
--                 out ("]")
--            out ("}")
