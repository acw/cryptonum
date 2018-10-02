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
mod mul;
#[macro_use]
mod shifts;
#[macro_use]
mod sub;

use self::add::{addition,unsafe_addition};
use self::base::CryptoNum;
use self::cmp::compare;
use self::codec::{Encoder,Decoder,raw_decoder};
use self::div::{DivMod,get_number_size};
use self::formatter::tochar;
use self::modmul::ModMul;
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

macro_rules! modsq_impls
{
    ($name: ident) => {
    }
}

include!("invoc.rs");
