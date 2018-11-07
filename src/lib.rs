#[cfg(test)]
#[macro_use]
extern crate quickcheck;

pub mod signed;
pub mod unsigned;
#[cfg(test)]
pub mod testing;

#[cfg(test)]
mod arithmetic {
    use super::signed::*;
    use super::unsigned::*;

    quickcheck! {
        fn commutivity_signed_add(x: I576, y: I576) -> bool {
            (&x + &y) == (&y + &x)
        }
        fn commutivity_unsigned_add(x: U576, y: U576) -> bool {
            (&x + &y) == (&y + &x)
        }
        fn commutivity_unsigned_mul(x: U192, y: U192) -> bool {
            (&x * &y) == (&y * &x)
        }

        fn associativity_unsigned_add(x: U192, y: U192, z: U192) -> bool {
            (U256::from(&x) + (&y + &z)) == ((&x + &y) + U256::from(&z))
        }
        fn associativity_unsigned_mul(x: U192, y: U192, z: U192) -> bool {
            (U384::from(&x) * (&y * &z)) == ((&x * &y) * U384::from(&z))
        }

        fn identity_signed_add(x: I576) -> bool {
            (&x + I576::zero()) == I640::from(&x)
        }
        fn identity_unsigned_add(x: U576) -> bool {
            (&x + U576::zero()) == U640::from(&x)
        }
        fn identity_unsigned_mul(x: U192) -> bool {
            (&x * U192::from(1u64)) == U384::from(&x)
        }

        fn additive_inverse(x: I576) -> bool {
            (&x + x.negate()) == I640::zero()
        }
        fn distribution(x: U192, y: U192, z: U192) -> bool {
            (U256::from(&x) * (&y + &z)) == U512::from((&x * &y) + (&x * &z))
        }
    } 
}