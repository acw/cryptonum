#[macro_use]
mod add;
#[macro_use]
mod base;
#[macro_use]
mod compare;
#[macro_use]
mod conversion;
#[macro_use]
mod egcd;
#[macro_use]
mod modinv;
#[macro_use]
mod shift;
#[macro_use]
mod subtraction;

use std::cmp::{Ord,Ordering,PartialOrd};
use std::fmt;
use std::ops::{Add,AddAssign};
use std::ops::{Shl,ShlAssign,Shr,ShrAssign};
use std::ops::{Sub,SubAssign};
use unsigned::*;

pub use self::egcd::EGCD;

include!("invoc.rs");