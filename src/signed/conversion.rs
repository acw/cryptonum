macro_rules! generate_base_conversions
{
    ($sname: ident, $name: ident) => {
        generate_base_type_convert!($sname, $name, i8,    u8);
        generate_base_type_convert!($sname, $name, i16,   u16);
        generate_base_type_convert!($sname, $name, i32,   u32);
        generate_base_type_convert!($sname, $name, i64,   u64);
        generate_base_type_convert!($sname, $name, isize, usize);

        impl From<i128> for $sname {
            fn from(x: i128) -> $sname {
                $sname{ negative: x < 0, value: $name::from(x.abs() as u128) }
            }
        }

        impl From<u64> for $sname {
            fn from(x: u64) -> $sname {
                $sname{ negative: false, value: $name::from(x) }
            }
        }
    };
}

macro_rules! generate_base_type_convert
{
    ($sname: ident, $name: ident, $sbase: ident, $base: ident) => {
        impl From<$sbase> for $sname {
            fn from(x: $sbase) -> $sname {
                $sname {
                    negative: x < 0,
                    value: $name::from(x.abs() as $base)
                }
            }
        }

        impl From<$sname> for $sbase {
            fn from(x: $sname) -> $sbase {
                let signum = if x.negative { -1 } else { 1 };
                signum * ($base::from(x.value) as $sbase)
            }
        }

        impl<'a> From<&'a $sname> for $sbase {
            fn from(x: &$sname) -> $sbase {
                let signum = if x.negative { -1 } else { 1 };
                signum * ($base::from(&x.value) as $sbase)
            }
        }
    }
}

#[cfg(test)]
macro_rules! generate_sigconversion_tests
{
    ($sname: ident, $name: ident, $lname: ident) => {
        #[cfg(test)]
        mod $lname {
            use super::super::super::*;
            use std::convert::From;

            quickcheck! {
                fn conversion_i8(   x: i8)    -> bool { x == i8::from($sname::from(x))    }
                fn conversion_i16(  x: i16)   -> bool { x == i16::from($sname::from(x))   }
                fn conversion_i32(  x: i32)   -> bool { x == i32::from($sname::from(x))   }
                fn conversion_i64(  x: i64)   -> bool { x == i64::from($sname::from(x))   }
                fn conversion_isize(x: isize) -> bool { x == isize::from($sname::from(x)) }
            }
        }
    }
}

macro_rules! conversion_impls
{
    ($sname: ident, $name: ident, $sother: ident, $other: ident) => {
        impl<'a> From<&'a $sother> for $sname {
            fn from(x: &$sother) -> $sname {
                $sname {
                    negative: x.negative,
                    value: $name::from(&x.value)
                }
            }
        }

        impl<'a> From<&'a $sname> for $sother {
            fn from(x: &$sname) -> $sother {
                $sother {
                    negative: x.negative,
                    value: $other::from(&x.value)
                }
            }
        }

        impl From<$sother> for $sname {
            fn from(x: $sother) -> $sname {
                $sname::from(&x)
            }
        }

        impl From<$sname> for $sother {
            fn from(x: $sname) -> $sother {
                $sother::from(&x)
            }
        }
    };
}