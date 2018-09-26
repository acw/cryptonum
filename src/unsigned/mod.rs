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
mod mul;
#[macro_use]
mod shifts;
#[macro_use]
mod sub;

use self::add::{addition,unsafe_addition};
use self::base::CryptoNum;
use self::cmp::compare;
use self::div::{DivMod,get_number_size};
use self::codec::{Encoder,Decoder,raw_decoder};
use self::mul::multiply;
use self::shifts::{shiftl,shiftr};
use self::sub::subtract;
use std::cmp::{Ordering,min};
use std::ops::{Add,AddAssign};
use std::ops::{Div,Mul,Rem};
use std::ops::{Shl,ShlAssign,Shr,ShrAssign};
use std::ops::{Sub,SubAssign};

#[cfg(test)]
use std::fmt;

macro_rules! generate_number
{
    ($name: ident, $size: expr) => {
        generate_base!($name, $size);
        generate_base_conversions!($name);
        generate_codec!($name);

        cmp_impls!($name);

        subtraction_impls!($name, $size);
        shift_impls!($name, $size);
    };
    ($name: ident, $size: expr, $plus1: ident, $times2: ident) => {
        generate_number!($name, $size);
        addition_impls!($name, $plus1);
        multiply_impls!($name, $times2);
        div_impls!($name, $times2);
    };
    ($name: ident, $size: expr, $plus1: ident, $times2: ident, $big: ident, $bar: ident) => {
        generate_number!($name, $size, $plus1, $times2);
        barrett_impl!($bar, $name, $plus1, $times2, $big);
    }
}

include!("invoc.rs");