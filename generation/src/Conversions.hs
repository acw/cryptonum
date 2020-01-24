{-# LANGUAGE QuasiQuotes #-}
module Conversions(
    conversions
  , signedConversions
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
  isUnsigned = True,
  generator = declareConversions,
  testCase = Nothing
}

signedConversions :: File
signedConversions = File {
  predicate = \ _ _ -> True,
  outputName = "sconversions",
  isUnsigned = False,
  generator = declareSignedConversions,
  testCase = Nothing
}

declareConversions :: Word -> [Word] -> SourceFile Span
declareConversions bitsize otherSizes =
  let sname      = mkIdent ("U" ++ show bitsize)
      entries    = bitsize `div` 64
      u8_prims   = buildPrimitives sname (mkIdent "u8")  entries
      u16_prims  = buildPrimitives sname (mkIdent "u16") entries
      u32_prims  = buildPrimitives sname (mkIdent "u32") entries
      u64_prims  = buildPrimitives sname (mkIdent "u64") entries
      usz_prims  = buildPrimitives sname (mkIdent "usize") entries
      u128_prims = generateU128Primitives sname entries
      i8_prims   = generateSignedPrims sname (mkIdent "u8")  (mkIdent "i8")
      i16_prims  = generateSignedPrims sname (mkIdent "u16") (mkIdent "i16")
      i32_prims  = generateSignedPrims sname (mkIdent "u32") (mkIdent "i32")
      i64_prims  = generateSignedPrims sname (mkIdent "u64") (mkIdent "i64")
      isz_prims  = buildPrimitives sname (mkIdent "isize") entries
      i128_prims = generateI128Primitives sname
      others     = generateCryptonumConversions bitsize otherSizes
  in [sourceFile|
       use core::convert::{From,TryFrom};
       use crate::CryptoNum;
       use crate::ConversionError;
       #[cfg(test)]
       use quickcheck::quickcheck;
       use super::super::*;

       $@{u8_prims}
       $@{u16_prims}
       $@{u32_prims}
       $@{u64_prims}
       $@{usz_prims}
       $@{u128_prims}

       $@{i8_prims}
       $@{i16_prims}
       $@{i32_prims}
       $@{i64_prims}
       $@{isz_prims}
       $@{i128_prims}

       $@{others}

       #[cfg(test)]
       quickcheck! {
         fn u8_recovers(x: u8) -> bool {
            x == u8::try_from($$sname::from(x)).unwrap()
         }
         fn u16_recovers(x: u16) -> bool {
            x == u16::try_from($$sname::from(x)).unwrap()
         }
         fn u32_recovers(x: u32) -> bool {
            x == u32::try_from($$sname::from(x)).unwrap()
         }
         fn u64_recovers(x: u64) -> bool {
            x == u64::try_from($$sname::from(x)).unwrap()
         }
         fn usize_recovers(x: usize) -> bool {
            x == usize::try_from($$sname::from(x)).unwrap()
         }
         fn u128_recovers(x: u128) -> bool {
            x == u128::try_from($$sname::from(x)).unwrap()
         }
       }
     |]

declareSignedConversions :: Word -> [Word] -> SourceFile Span
declareSignedConversions bitsize otherSizes =
  let sname      = mkIdent ("I" ++ show bitsize)
      uname      = mkIdent ("U" ++ show bitsize)
      entries    = bitsize `div` 64
      u8_prims   = buildUSPrimitives sname (mkIdent "u8")
      u16_prims  = buildUSPrimitives sname (mkIdent "u16")
      u32_prims  = buildUSPrimitives sname (mkIdent "u32")
      u64_prims  = buildUSPrimitives sname (mkIdent "u64")
      usz_prims  = buildUSPrimitives sname (mkIdent "usize")
      i8_prims   = buildSSPrimitives sname uname (mkIdent "i8")
      i16_prims  = buildSSPrimitives sname uname (mkIdent "i16")
      i32_prims  = buildSSPrimitives sname uname (mkIdent "i32")
      i64_prims  = buildSSPrimitives sname uname (mkIdent "i64")
      isz_prims  = buildSSPrimitives sname uname (mkIdent "isize")
      s128_prims = generateS128Primitives sname uname entries
      others     = generateSignedCryptonumConversions bitsize otherSizes
  in [sourceFile|
       use core::convert::{From,TryFrom};
       use core::{i8,i16,i32,i64,isize};
       use crate::CryptoNum;
       use crate::ConversionError;
       use crate::signed::*;
       use crate::unsigned::*;
       #[cfg(test)]
       use quickcheck::quickcheck;

       $@{u8_prims}
       $@{u16_prims}
       $@{u32_prims}
       $@{u64_prims}
       $@{usz_prims}

       $@{i8_prims}
       $@{i16_prims}
       $@{i32_prims}
       $@{i64_prims}
       $@{isz_prims}
       $@{s128_prims}

       $@{others}

       #[cfg(test)]
       quickcheck! {
         fn u8_recovers(x: u8) -> bool {
            x == u8::try_from($$sname::from(x)).unwrap()
         }
         fn u16_recovers(x: u16) -> bool {
            x == u16::try_from($$sname::from(x)).unwrap()
         }
         fn u32_recovers(x: u32) -> bool {
            x == u32::try_from($$sname::from(x)).unwrap()
         }
         fn u64_recovers(x: u64) -> bool {
            x == u64::try_from($$sname::from(x)).unwrap()
         }
         fn usize_recovers(x: usize) -> bool {
            x == usize::try_from($$sname::from(x)).unwrap()
         }
         fn u128_recovers(x: u128) -> bool {
            x == u128::try_from($$sname::from(x)).unwrap()
         }
         fn i8_recovers(x: i8) -> bool {
            x == i8::try_from($$sname::from(x)).unwrap()
         }
         fn i16_recovers(x: i16) -> bool {
            x == i16::try_from($$sname::from(x)).unwrap()
         }
         fn i32_recovers(x: i32) -> bool {
            x == i32::try_from($$sname::from(x)).unwrap()
         }
         fn i64_recovers(x: i64) -> bool {
            x == i64::try_from($$sname::from(x)).unwrap()
         }
         fn isize_recovers(x: isize) -> bool {
            x == isize::try_from($$sname::from(x)).unwrap()
         }
         fn i128_recovers(x: i128) -> bool {
            x == i128::try_from($$sname::from(x)).unwrap()
         }
       }
    |]
 
generateU128Primitives :: Ident -> Word -> [Item Span]
generateU128Primitives sname entries = [
    [item|impl From<u128> for $$sname {
         fn from(x: u128) -> Self {
            let mut res = $$sname::zero();
            res.value[0] = x as u64;
            res.value[1] = (x >> 64) as u64;
            res
         }
       }|]
  , [item|impl TryFrom<$$sname> for u128 {
         type Error = ConversionError;

         fn try_from(x: $$sname) -> Result<u128,ConversionError> {
            let mut good_conversion = true;
            let mut res;

            res = (x.value[1] as u128) << 64;
            res |= x.value[0] as u128;

            $@{testZeros}
            if good_conversion {
                Ok(res)
            } else {
                Err(ConversionError::Overflow)
            }
         }
       }|]
  , [item|impl<'a> TryFrom<&'a $$sname> for u128 {
         type Error = ConversionError;

         fn try_from(x: &$$sname) -> Result<u128,ConversionError> {
            let mut good_conversion = true;
            let mut res;

            res = (x.value[1] as u128) << 64;
            res |= x.value[0] as u128;

            $@{testZeros}
            if good_conversion {
                Ok(res)
            } else {
                Err(ConversionError::Overflow)
            }
         }
       }|]
  ]
 where
  testZeros = map (zeroTest . toLit) [2..entries-1]
  zeroTest i =
    [stmt| good_conversion &= x.value[$$(i)] == 0; |]

buildPrimitives :: Ident -> Ident -> Word -> [Item Span]
buildPrimitives sname tname entries = [
    [item|impl From<$$tname> for $$sname {
      fn from(x: $$tname) -> Self {
        let mut res = $$sname::zero();
        res.value[0] = x as u64;
        res
      }
    }|]
  , [item|impl TryFrom<$$sname> for $$tname {
      type Error = ConversionError;

      fn try_from(x: $$sname) -> Result<Self,ConversionError> {
        let mut good_conversion = true;
        let res = x.value[0] as $$tname;

        $@{testZeros}
        if good_conversion {
          Ok(res)
        } else {
          Err(ConversionError::Overflow)
        }
      }
    }|]
  , [item|impl<'a> TryFrom<&'a $$sname> for $$tname {
      type Error = ConversionError;

      fn try_from(x: &$$sname) -> Result<Self,ConversionError> {
        let mut good_conversion = true;
        let res = x.value[0] as $$tname;

        $@{testZeros}
        if good_conversion {
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
    [stmt| good_conversion &= x.value[$$(i)] == 0; |]

generateSignedPrims :: Ident -> Ident -> Ident -> [Item Span]
generateSignedPrims sname unsigned signed = [
    [item|impl TryFrom<$$signed> for $$sname {
      type Error = ConversionError;

      fn try_from(x: $$signed) -> Result<Self,ConversionError> {
        let mut res = $$sname::zero();
        res.value[0] = x as u64;
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
        let uns = $$unsigned::try_from(x)?;
        Ok($$signed::try_from(uns)?)
      }
    }|]
  , [item|impl<'a> TryFrom<&'a $$sname> for $$signed {
      type Error = ConversionError;

      fn try_from(x: &$$sname) -> Result<Self,ConversionError> {
        let uns = $$unsigned::try_from(x)?;
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
        res.value[0] = x as u64;
        res.value[1] = ((x as u128) >> 64) as u64;
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
        let uns = u128::try_from(x)?;
        Ok(i128::try_from(uns)?)
      }
    }|]
  , [item|impl<'a> TryFrom<&'a $$sname> for i128 {
      type Error = ConversionError;

      fn try_from(x: &$$sname) -> Result<Self,ConversionError> {
        let uns = u128::try_from(x)?;
        Ok(i128::try_from(uns)?)
      }
    }|]
   ]

generateCryptonumConversions :: Word -> [Word] -> [Item Span]
generateCryptonumConversions source = concatMap convert
 where
   sName = mkIdent ("U" ++ show source)
   --
   convert target =
     let tName = mkIdent ("U" ++ show target)
         sEntries = toLit (source `div` 64)
         tEntries = toLit (target `div` 64)
     in case compare source target of
          LT -> [
            [item| 
             impl<'a> From<&'a $$sName> for $$tName {
               fn from(x: &$$sName) -> $$tName {
                 let mut res = $$tName::zero();
                 res.value[0..$$(sEntries)].copy_from_slice(&x.value);
                 res
               }
             }
            |],
            [item|
             impl From<$$sName> for $$tName {
               fn from(x: $$sName) -> $$tName {
                 $$tName::from(&x)
               }
             }
            |]
            ]
          EQ -> []
          GT -> [
            [item|
              impl<'a> TryFrom<&'a $$sName> for $$tName {
                type Error = ConversionError;

                fn try_from(x: &$$sName) -> Result<$$tName, ConversionError> {
                  if x.value.iter().skip($$(tEntries)).all(|x| *x == 0) {
                    let mut res = $$tName::zero();
                    res.value.copy_from_slice(&x.value[0..$$(tEntries)]);
                    Ok(res)
                  } else {
                    Err(ConversionError::Overflow)
                  }
                }
              }
            |],
            [item|
              impl TryFrom<$$sName> for $$tName {
                type Error = ConversionError;

                fn try_from(x: $$sName) -> Result<$$tName, ConversionError> {
                  $$tName::try_from(&x)
                }
              }
            |]
            ]

buildUSPrimitives :: Ident -> Ident -> [Item Span]
buildUSPrimitives sname prim = [
    [item|
        impl From<$$prim> for $$sname {
          fn from(x: $$prim) -> $$sname {
            let mut base = $$sname::zero();
            base.contents.value[0] = x as u64;
            base
          }
        }
    |]
  , [item|
        impl<'a> TryFrom<&'a $$sname> for $$prim {
          type Error = ConversionError;

          fn try_from(x: &$$sname) -> Result<$$prim, ConversionError> {
              if (x.contents.value)[1..].iter().any(|v| *v != 0) {
                return Err(ConversionError::Overflow);
              }
              let res64 = x.contents.value[0];
              if res64 & 0x8000_0000_0000_0000 != 0 {
                return Err(ConversionError::Overflow);
              }
              Ok(res64 as $$prim)
          }
        }
    |]
  , [item|
        impl TryFrom<$$sname> for $$prim {
          type Error = ConversionError;

          fn try_from(x: $$sname) -> Result<$$prim, ConversionError> {
            $$prim::try_from(&x)
          }
        }
    |]
  ]

buildSSPrimitives :: Ident -> Ident -> Ident -> [Item Span]
buildSSPrimitives sname uname prim = [
    [item|
        impl From<$$prim> for $$sname {
          fn from(x: $$prim) -> $$sname {
            let mut ures = $$uname::zero();
            let topbits = if x < 0 { 0xFFFF_FFFF_FFFF_FFFF } else { 0 };
            for x in ures.value.iter_mut() {
              *x = topbits;
            }
            ures.value[0] = (x as i64) as u64;
            $$sname{ contents: ures }
          }
        }
    |]
  , [item|
        impl<'a> TryFrom<&'a $$sname> for $$prim {
          type Error = ConversionError;

          fn try_from(x: &$$sname) -> Result<$$prim, ConversionError> {
            let topbits = if x.is_negative() { 0xFFFF_FFFF_FFFF_FFFF } else { 0 };
            if x.contents.value[1..].iter().any(|v| *v != topbits) {
              return Err(ConversionError::Overflow);
            }
            let local_min = $$prim::MIN as i64;
            let local_max = $$prim::MAX as i64;
            let bottom    = x.contents.value[0] as i64;

            if (bottom > local_max) || (bottom < local_min) {
              Err(ConversionError::Overflow)
            } else {
              Ok(bottom as $$prim)
            }
          }
        }
    |]
  , [item|
        impl TryFrom<$$sname> for $$prim {
          type Error = ConversionError;

          fn try_from(x: $$sname) -> Result<$$prim, ConversionError> {
            $$prim::try_from(&x)
          }
        }
    |]
  ]

generateS128Primitives :: Ident -> Ident -> Word -> [Item Span]
generateS128Primitives sname uname entries = [
    [item|
      impl From<u128> for $$sname {
        fn from(x: u128) -> $$sname {
          $$sname{ contents: $$uname::from(x) }
        }
      }
    |],
    [item|
      impl From<i128> for $$sname {
        fn from(x: i128) -> $$sname {
          let mut basic = $$uname::from(x as u128);
          if x < 0 {
            for x in basic.value[2..].iter_mut() {
              *x = 0xFFFF_FFFF_FFFF_FFFF;
            }
          }
          $$sname{ contents: basic }
        }
      }
    |],
    [item|
      impl TryFrom<$$sname> for u128 {
        type Error = ConversionError;

        fn try_from(x: $$sname) -> Result<u128,ConversionError> {
          u128::try_from(&x)
        }
      }
    |],
    [item|
      impl TryFrom<$$sname> for i128 {
        type Error = ConversionError;

        fn try_from(x: $$sname) -> Result<i128,ConversionError> {
          i128::try_from(&x)
        }
      }
    |],
    [item|
      impl<'a> TryFrom<&'a $$sname> for u128 {
        type Error = ConversionError;

        fn try_from(x: &$$sname) -> Result<u128,ConversionError> {
          if x.is_negative() {
            return Err(ConversionError::Overflow);
          }
          u128::try_from(&x.contents)
        }
      }
    |],
    [item|
      impl<'a> TryFrom<&'a $$sname> for i128 {
        type Error = ConversionError;

        fn try_from(x: &$$sname) -> Result<i128,ConversionError> {
          let isneg = x.is_negative();
          let target_top = if isneg { 0xFFFF_FFFF_FFFF_FFFF } else { 0x0 };
          let mut worked = true;

          worked &= x.contents.value[2..].iter().all(|v| *v == target_top);
          worked &= (x.contents.value[1] >> 63 == 1) == isneg;

          let res = ((x.contents.value[1] as u128) << 64) | (x.contents.value[0] as u128);
          if worked {
            Ok(res as i128)
          } else {
            Err(ConversionError::Overflow)
          }
        }
      }
    |]
  ]

generateSignedCryptonumConversions :: Word -> [Word] -> [Item Span]
generateSignedCryptonumConversions source otherSizes = concatMap convert otherSizes
 where
   sName = mkIdent ("I" ++ show source)
   --
   convert target =
     let tsName = mkIdent ("I" ++ show target)
         tuName = mkIdent ("U" ++ show target)
         sEntries = toLit (source `div` 64)
         tEntries = toLit (target `div` 64)
     in case compare source target of
          LT -> [
           ]
          EQ -> [
            [item|
              impl TryFrom<$$tuName> for $$sName {
                type Error = ConversionError;

                fn try_from(x: $$tuName) -> Result<$$sName,ConversionError> {
                  let res = $$sName{ contents: x };

                  if res.is_negative() {
                    return Err(ConversionError::Overflow);
                  }

                  Ok(res)
                }
              }
            |],
            [item|
              impl<'a> TryFrom<&'a $$tuName> for $$sName {
                type Error = ConversionError;

                fn try_from(x: &$$tuName) -> Result<$$sName,ConversionError> {
                  $$sName::try_from(x.clone())
                }
              }
            |],
            [item|
              impl TryFrom<$$sName> for $$tuName {
                type Error = ConversionError;

                fn try_from(x: $$sName) -> Result<$$tuName,ConversionError> {
                  if x.is_negative() {
                    return Err(ConversionError::Overflow);
                  }
                  Ok(x.contents)
                }
              }
            |],
            [item|
              impl<'a> TryFrom<&'a $$sName> for $$tuName {
                type Error = ConversionError;

                fn try_from(x: &$$sName) -> Result<$$tuName,ConversionError> {
                  $$tuName::try_from(x.clone())
                }
              }
            |]
           ]
          GT -> [
            [item|
              impl From<$$tuName> for $$sName {
                fn from(x: $$tuName) -> $$sName {
                  $$sName::from(&x)
                }
              }
            |],
            [item|
              impl<'a> From<&'a $$tuName> for $$sName {
                fn from(x: &$$tuName) -> $$sName {
                  panic!("from1")
                }
              }
            |],
            [item|
              impl From<$$tsName> for $$sName {
                fn from(x: $$tsName) -> $$sName {
                  $$sName::from(&x)
                }
              }
            |],
            [item|
              impl<'a> From<&'a $$tsName> for $$sName {
                fn from(x: &$$tsName) -> $$sName {
                  panic!("from2")
                }
              }
            |]   
           ]

