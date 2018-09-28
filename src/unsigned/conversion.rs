macro_rules! generate_base_conversions
{
    ($name: ident) => {
        generate_base_type_convert!($name, u8);
        generate_base_type_convert!($name, u16);
        generate_base_type_convert!($name, u32);
        generate_base_type_convert!($name, u64);
        generate_base_type_convert!($name, usize);

        impl From<u128> for $name {
            fn from(x: u128) -> $name {
                let mut res = $name::zero();
                res.value[0] = x as u64;
                res.value[1] = (x >> 64) as u64;
                res
            }
        }
    };
    () => {};
}

#[cfg(test)]
macro_rules! generate_conversion_tests
{
    ($name: ident, $lname: ident) => {
        #[cfg(test)]
        mod $lname {
            use super::super::super::*;
            use std::convert::From;

            quickcheck! {
                fn conversion_u8(   x: u8)    -> bool { x == u8::from($name::from(x))    }
                fn conversion_u16(  x: u16)   -> bool { x == u16::from($name::from(x))   }
                fn conversion_u32(  x: u32)   -> bool { x == u32::from($name::from(x))   }
                fn conversion_u64(  x: u64)   -> bool { x == u64::from($name::from(x))   }
                fn conversion_usize(x: usize) -> bool { x == usize::from($name::from(x)) }
            }
        }
    }
}

macro_rules! conversion_impls
{
    ($name: ident, $other: ident) => {
        impl<'a> From<&'a $other> for $name {
            fn from(x: &$other) -> $name {
                let mut res = $name::zero();
                let     len = res.value.len();
                assert!(x.value.len() > res.value.len());
                res.value.copy_from_slice(&x.value[0..len]);
                res
            }
        }

        impl<'a> From<&'a $name> for $other {
            fn from(x: &$name) -> $other {
                let mut res = $other::zero();
                let     len = x.value.len();

                assert!(x.value.len() < res.value.len());
                res.value[0..len].copy_from_slice(&x.value);
                res
            }
        }

        impl From<$other> for $name {
            fn from(x: $other) -> $name {
                $name::from(&x)
            }
        }

        impl From<$name> for $other {
            fn from(x: $name) -> $other {
                $other::from(&x)
            }
        }
    };
}

macro_rules! generate_base_type_convert
{
    ($name: ident, $base: ident) => {
        impl From<$base> for $name {
            fn from(x: $base) -> $name {
                let mut res = $name::zero();
                res.value[0] = x as u64;
                res
            }
        }

        impl From<$name> for $base {
            fn from(x: $name) -> $base {
                x.value[0] as $base
            }
        }

        impl<'a> From<&'a $name> for $base {
            fn from(x: &$name) -> $base {
                x.value[0] as $base
            }
        }
    }
}