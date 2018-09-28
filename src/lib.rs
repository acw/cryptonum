#![recursion_limit="1024"]

#[cfg(test)]
#[macro_use]
extern crate quickcheck;

pub mod unsigned;
#[cfg(test)]
pub mod testing;