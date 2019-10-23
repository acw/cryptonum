{-# LANGUAGE QuasiQuotes #-}
module Conversions(
    conversions
  )
 where

import File
import Gen(toLit)
import Language.Rust.Data.Ident
import Language.Rust.Data.Position
import Language.Rust.Quote
import Language.Rust.Syntax

conversions :: File
conversions = File {
  predicate = \ _ _ -> True,
  outputName = "conversions",
  generator = declareConversions
}

declareConversions :: Word -> SourceFile Span
declareConversions bitsize =
  let sname      = mkIdent ("U" ++ show bitsize)
      entries    = bitsize `div` 64
      u8_prims   = buildPrimitives sname (mkIdent "u8")  entries
      u16_prims  = buildPrimitives sname (mkIdent "u16") entries
      u32_prims  = buildPrimitives sname (mkIdent "u32") entries
      u64_prims  = buildPrimitives sname (mkIdent "u64") entries
      u128_prims = generateU128Primitives sname entries
      i8_prims   = generateSignedPrims sname (mkIdent "u8")  (mkIdent "i8")
      i16_prims  = generateSignedPrims sname (mkIdent "u16") (mkIdent "i16")
      i32_prims  = generateSignedPrims sname (mkIdent "u32") (mkIdent "i32")
      i64_prims  = generateSignedPrims sname (mkIdent "u64") (mkIdent "i64")
      i128_prims = generateI128Primitives sname
  in [sourceFile|
       use core::convert::{From,TryFrom};
       use core::num::TryFromIntError;
       #[cfg(test)]
       use quickcheck::quickcheck;
       use super::$$sname;
       use crate::ConversionError;

       $@{u8_prims}
       $@{u16_prims}
       $@{u32_prims}
       $@{u64_prims}
       $@{u128_prims}

       $@{i8_prims}
       $@{i16_prims}
       $@{i32_prims}
       $@{i64_prims}
       $@{i128_prims}
     |]

generateU128Primitives :: Ident -> Word -> [Item Span]
generateU128Primitives sname entries = [
    [item|impl From<u128> for $$sname {
         fn from(x: u128) -> Self {
            let mut res = $$sname::zero;
            res[0] = x as u64;
            res[1] = (x >> 64) as u64;
            res
         }
       }|]
  , [item|impl TryFrom<$$sname> for u128 {
         type Error = ConversionError;

         fn try_from(x: $$sname) -> Result<u128,ConversionError> {
            let mut goodConversion = true;
            let mut res = 0;

            res = (x.values[1] as u128) << 64;
            res |= x.values[0] as u128;

            $@{testZeros}
            if goodConversion {
                Ok(res)
            } else {
                Err(ConversionError::Overflow);
            }
         }
       }|]
  , [item|impl<'a> TryFrom<&'a $$sname> for u128 {
         type Error = ConversionError;

         fn try_from(x: &$$sname) -> Result<u128,ConversionError> {
            let mut goodConversion = true;
            let mut res = 0;

            res = (x.values[1] as u128) << 64;
            res |= x.values[0] as u128;

            $@{testZeros}
            if goodConversion {
                Ok(res)
            } else {
                Err(ConversionError::Overflow());
            }
         }
       }|]
  ]
 where
  testZeros = map (zeroTest . toLit) [2..entries-1]
  zeroTest i =
    [stmt| goodConversion &= x.values[$$(i)] == 0; |]

buildPrimitives :: Ident -> Ident -> Word -> [Item Span]
buildPrimitives sname tname entries = [
    [item|impl From<$$tname> for $$sname {
      fn from(x: $$tname) -> Self {
        let mut res = $$sname::zero();
        res.values[0] = x as u64;
        res
      }
    }|]
  , [item|impl TryFrom<$$sname> for $$tname {
      type Error = ConversionError;

      fn try_from(x: $$sname) -> Result<Self,ConversionError> {
        let mut goodConversion = true;
        let mut res = 0;

        res = x.values[0] as $$tname;

        $@{testZeros}
        if goodConversion {
          Ok(res)
        } else {
          Err(ConversionError::Overflow)
        }
      }
    }|]
  , [item|impl<'a> TryFrom<&'a $$sname> for $$tname {
      type Error = ConversionError;

      fn try_from(x: &$$sname) -> Result<Self,ConversionError> {
        let mut goodConversion = true;
        let mut res = 0;

        res = x.values[0] as $$tname;

        $@{testZeros}
        if goodConversion {
          Ok(res)
        } else {
          Err(ConversionError::Overflow)
        }
      }
    }|]
  ]
 where
  testZeros = map (zeroTest . toLit) [1..entries-1]
  zeroTest i =
    [stmt| goodConversion &= x.values[$$(i)] == 0; |]

generateSignedPrims :: Ident -> Ident -> Ident -> [Item Span]
generateSignedPrims sname unsigned signed = [
    [item|impl TryFrom<$$signed> for $$sname {
      type Error = ConversionError;

      fn try_from(x: $$signed) -> Result<Self,ConversionError> {
        let mut res = $$sname::zero();
        res.values[0] = x as u64;
        if x < 0 {
          Err(ConversionError::NegativeToUnsigned)
        } else {
          Ok(res)
        }
      }
    }|]
  , [item|impl TryFrom<$$sname> for $$signed {
      type Error = ConversionError;

      fn try_from(x: $$sname) -> Result<Self,ConversionError> {
        let uns = $$unsigned::from(x)?;
        Ok($$signed::try_from(uns)?)
      }
    }|]
  , [item|impl<'a> TryFrom<&'a $$sname> for $$signed {
      type Error = ConversionError;

      fn try_from(x: &$$sname) -> Result<Self,ConversionError> {
        let uns = $$unsigned::from(x)?;
        Ok($$signed::try_from(uns)?)
      }
    }|]
  ]

generateI128Primitives :: Ident -> [Item Span]
generateI128Primitives sname = [
    [item|impl TryFrom<i128> for $$sname {
      type Error = ConversionError;

      fn try_from(x: i128) -> Result<Self,ConversionError> {
        let mut res = $$sname::zero();
        res.values[0] = x as u64;
        res.values[1] = ((x as u128) >> 64) as u64;
        if x < 0 {
          Err(ConversionError::NegativeToUnsigned)
        } else {
          Ok(res)
        }
      }
    }|]
  , [item|impl TryFrom<$$sname> for i128 {
      type Error = ConversionError;

      fn try_from(x: $$sname) -> Result<Self,ConversionError> {
        let uns = u128::from(x)?;
        Ok(i128::try_from(uns)?)
      }
    }|]
  , [item|impl<'a> TryFrom<&'a $$sname> for i128 {
      type Error = ConversionError;

      fn try_from(x: &$$sname) -> Result<Self,ConversionError> {
        let uns = u128::from(x)?;
        Ok(i128::try_from(uns)?)
      }
    }|]
   ]
