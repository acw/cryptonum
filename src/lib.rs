#![no_std]
pub mod signed;
pub mod unsigned;

/// A trait definition for large numbers.
pub trait CryptoNum {
    /// Generate a new value of the given type.
    fn zero() -> Self;
    /// Test if the number is zero.
    fn is_zero(&self) -> bool;
    /// Test if the number is even.
    fn is_even(&self) -> bool;
    /// Test if the number is odd.
    fn is_odd(&self) -> bool;
    /// The size of this number in bits.
    fn bit_length() -> usize;
    /// Mask off the high parts of the number. In particular, it
    /// zeros the bits above (len * 64).
    fn mask(&mut self, len: usize);
    /// Test if the given bit is zero, where bits are numbered in
    /// least-significant order (0 is the LSB, etc.).
    fn testbit(&self, bit: usize) -> bool;
}

