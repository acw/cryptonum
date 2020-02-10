#![cfg_attr(not(test),no_std)]
pub mod signed;
pub mod unsigned;
#[cfg(test)]
mod testing;

use core::num::TryFromIntError;

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
    fn from_bytes(bytes: &[u8]) -> Self;
    /// Write the cryptonum into the provide slice. If the provided slice
    /// is greater than or equal to `n` bits in length, `to_bytes` will
    /// write to the first `n` bits. If the slice is less than `n` bits
    /// in length, `to_bytes` will write the lower-order bits that fit
    /// into the provided slice.
    fn to_bytes(&self, bytes: &mut [u8]);
}

/// Provides the ability to do a simultaneous division and modulus operation;
/// this is used as the implementation of division and multiplication, and
/// so you can save time doing both at once if you need them.
///
/// WARNING: There has been some effort made to make this have a constant-time
/// implementation, but it does use a single conditional inside an otherwise-
/// constant time loop. There may be unforeseen timing effects of this, or
/// the compiler may do something funny to "optimize" some math.
pub trait DivMod: Sized {
    /// Divide and modulus as a single operation. The first element of the tuple
    /// is the quotient, the second is the modulus.
    fn divmod(&self, rhs: &Self) -> (Self, Self);
}

/// Provides support for a variety of modular mathematical operations, as beloved
/// by cryptographers. Note that modular inversion and GCD calculations are shoved
/// off into another trait, because they operate on slightly different number
/// types.
pub trait ModularOperations<Modulus=Self> {
    // reduce the current value by the provided modulus
    fn reduce(&self, m: &Modulus) -> Self;
    // multiply this value by the provided one, modulo the modulus
    fn modmul(&self, rhs: &Self, m: &Modulus) -> Self;
    // square the provided number, modulo the modulus
    fn modsq(&self, m: &Modulus) -> Self;
    // modular exponentiation!
    fn modexp(&self, e: &Self, m: &Modulus) -> Self;

}

/// Provide support for modular inversion and GCD operations, which are useful
/// here and there. We provide default implementations for `modinv` and
/// `gcd_is_one`, based on the implementation of `egcd`. The built-in versions
/// explicitly define the latter, though, to improve performance.
pub trait ModularInversion: Sized {
    type Signed;

    fn modinv(&self, phi: &Self) -> Option<Self>;
    fn egcd(&self, rhs: &Self) -> (Self::Signed, Self::Signed, Self::Signed);
    fn gcd_is_one(&self, b: &Self) -> bool;
}

/// An error in conversion of large numbers (either to primitives or to other numbers
#[derive(Debug)]
pub enum ConversionError {
    NegativeToUnsigned,
    Overflow
}

impl From<TryFromIntError> for ConversionError {
    fn from(_: TryFromIntError) -> ConversionError {
        ConversionError::Overflow
    }
}
