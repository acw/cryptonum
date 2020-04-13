{-# LANGUAGE QuasiQuotes #-}
module Signed(signedBaseOps)
 where

import Language.Rust.Data.Ident
import Language.Rust.Data.Position
import Language.Rust.Quote
import Language.Rust.Syntax
import RustModule

signedBaseOps :: RustModule
signedBaseOps = RustModule {
    predicate = const (const True),
    suggested = const [],
    outputName = "base",
    isUnsigned = False,
    generator = declareSigned,
    testCase = Nothing
}

declareSigned :: Word -> [Word] -> SourceFile Span
declareSigned bitsize _ =
    let sname = mkIdent ("I" ++ show bitsize)
        uname = mkIdent ("U" ++ show bitsize)
    in [sourceFile|
        use core::fmt;
        use core::ops::{Neg, Not};
        use crate::CryptoNum;
        use crate::unsigned::$$uname;
        use quickcheck::{Arbitrary,Gen};
        #[cfg(test)]
        use quickcheck::quickcheck;

        #[derive(Clone)]
        pub struct $$sname {
            pub(crate) contents: $$uname,
        }

        impl $$sname {
            pub fn is_negative(&self) -> bool {
                self.contents.value[self.contents.value.len()-1] & 0x8000_0000_0000_0000 != 0
            }
        }

        impl Neg for $$sname {
            type Output = $$sname;

            fn neg(mut self) -> $$sname {
                for x in self.contents.value.iter_mut() {
                    *x = !*x;
                }
                let one = $$uname::from(1u64);
                self.contents += one;
                self
            }
        }

        impl<'a> Neg for &'a $$sname {
            type Output = $$sname;

            fn neg(self) -> $$sname {
                let res = self.clone();
                res.neg()
            }
        }

        impl Not for $$sname {
            type Output = $$sname;

            fn not(mut self) -> $$sname {
                for x in self.contents.value.iter_mut() {
                    *x = !*x;
                }
                self
            }
        }

        impl<'a> Not for &'a $$sname {
            type Output = $$sname;

            fn not(self) -> $$sname {
                self.clone().not()
            }
        }

        impl fmt::Debug for $$sname {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{:?}", self.contents)
            }
        }

        impl fmt::UpperHex for $$sname {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                let mut temp = self.clone();

                if temp.contents.value[temp.contents.value.len()-1] >> 63 == 1 {
                    write!(f, "-")?;
                    temp = !temp;
                }

                write!(f, "{:X}", temp.contents)
            }
        }

        impl fmt::LowerHex for $$sname {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                let mut temp = self.clone();

                if temp.contents.value[temp.contents.value.len()-1] >> 63 == 1 {
                    write!(f, "-")?;
                    temp = !temp;
                }

                write!(f, "{:x}", temp.contents)
            }
        }

        impl Arbitrary for $$sname {
            fn arbitrary<G: Gen>(g: &mut G) -> $$sname {
                $$sname{
                    contents: $$uname::arbitrary(g),
                }
            }
        }

        impl CryptoNum for $$sname {
            fn zero() -> $$sname {
                $$sname{ contents: $$uname::zero() }
            }
            fn is_zero(&self) -> bool {
                self.contents.is_zero()
            }
            fn is_even(&self) -> bool {
                self.contents.is_even()
            }
            fn is_odd(&self) -> bool {
                self.contents.is_odd()
            }
            fn bit_length() -> usize {
                $$uname::bit_length()
            }
            fn mask(&mut self, len: usize) {
                self.contents.mask(len);
            }
            fn testbit(&self, bit: usize) -> bool {
                self.contents.testbit(bit)
            }
            fn from_bytes(bytes: &[u8]) -> $$sname {
                $$sname{ contents: $$uname::from_bytes(bytes) }
            }
            fn to_bytes(&self, bytes: &mut [u8]) {
                self.contents.to_bytes(bytes);
            }
        }

        #[cfg(test)]
        quickcheck! {
            fn double_not(x: $$sname) -> bool {
                x == !!&x
            }

            fn double_neg(x: $$sname) -> bool {
                x == --&x
            }
        }
    |]