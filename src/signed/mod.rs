//! This module includes a large number of signed integer types for very
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
pub use self::modinv::ModInv;

include!("invoc.rs");