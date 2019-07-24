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
    /// Convert a slice into a CryptoNum, assuming big-endian memory layout.
    /// If you pass in a slice bigger than the bit size of the numeric type,
    /// this will assume that the number is in the first `n` bits of the
    /// memory layout. If you pass in a smaller buffer, it will use the bits
    /// available as the low `n` bits of the number.
    fn from_bytes(&self, bytes: &[u8]) -> Self;
    /// Write the cryptonum into the provide slice. If the provided slice
    /// is greater than or equal to `n` bits in length, `to_bytes` will
    /// write to the first `n` bits. If the slice is less than `n` bits
    /// in length, `to_bytes` will write the lower-order bits that fit
    /// into the provided slice.
    fn to_bytes(&self, bytes: &mut [u8]);
}

