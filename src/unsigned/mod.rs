//! This module includes a large number of unsigned integer types for very
//! large integers, designed to try to match good performance with a high
//! assurance threshold.
//! 
//! The types provided in this module, and the functions available for each
//! of those types, is derived from standard bit widths for RSA, DSA, and
//! Elliptic Curve encryption schemes. If this library does not include a
//! function you would like for another cryptographic scheme, please reach
//! out to the authors; in many cases, the relevant code can be automatically
//! generated.
//! 
//! For performance reasons, we also include support for Barrett reduction,
//! which should improve the speed of modular reduction of large numbers for
//! those cases in which you will be frequently performing modulo operations
//! using the same modulus.
#[macro_use]
mod add;
#[macro_use]
mod barrett;
#[macro_use]
mod base;
#[macro_use]
mod cmp;
#[macro_use]
mod codec;
#[macro_use]
mod conversion;
#[macro_use]
mod div;
#[macro_use]
mod formatter;
#[macro_use]
mod modexp;
#[macro_use]
mod modmul;
#[macro_use]
mod modsq;
#[macro_use]
mod mul;
#[macro_use]
mod shifts;
#[macro_use]
mod square;
#[macro_use]
mod sub;

pub use self::base::CryptoNum;
pub use self::codec::{Encoder,Decoder};
pub use self::div::DivMod;
pub use self::modexp::ModExp;
pub use self::modmul::ModMul;
pub use self::modsq::ModSquare;
pub use self::square::Square;

pub(crate) use self::add::unsafe_addition;

use self::add::addition;
use self::cmp::compare;
use self::codec::raw_decoder;
use self::div::get_number_size;
use self::formatter::tochar;
use self::mul::multiply;
use self::shifts::{shiftl,shiftr};
use self::sub::subtract;
use std::cmp::{Ordering,min};
use std::fmt;
use std::fmt::Write;
use std::ops::{Add,AddAssign};
use std::ops::{Div,Mul,Rem};
use std::ops::{Shl,ShlAssign,Shr,ShrAssign};
use std::ops::{Sub,SubAssign};

#[cfg(test)]
use quickcheck::{Arbitrary,Gen};

macro_rules! base_impls
{
    ($name: ident, $size: expr) => {
        generate_base!($name, $size);
        generate_base_conversions!($name);
        generate_codec!($name);
        generate_formatter!($name);
        cmp_impls!($name);
    }
}

include!("invoc.rs");
