pub trait SquareRoot {
    /// Compute the integer square root of the given value. The integer square
    /// root is the value Z such that the real root R satisfies Z <= R < Z+1.
    fn sqrt(&self) -> Self;
}

macro_rules! sqrt_impls
{
    ($name: ident) => {
        impl SquareRoot for $name {
            fn sqrt(&self) -> Self {
                let mut num = self.clone();
                let mut res = $name::zero();
                let mut bit = $name::from(1u64) << ($name::bit_length() - 2);

                while bit > num {
                    bit >>= 2;
                }

                while !bit.is_zero() {
                    let mut resbit = res.clone();

                    resbit += &bit;
                    if num >= resbit {
                        num -= resbit;
                        res += &bit << 1;
                    }

                    res >>= 1;
                    bit >>= 2;
                }

                res
            }
        }
    }
}

#[cfg(test)]
macro_rules! generate_sqrt_tests {
    ($name: ident, $lname: ident) => {
        #[test]
        fn $lname() {
            generate_sqrt_tests!(body $name, $lname);
        }
    };
    (ignore $name: ident, $lname: ident) => {
        #[test]
        #[ignore]
        fn $lname() {
            generate_sqrt_tests!(body $name, $lname);
        }
    };
    (body $name: ident, $lname: ident) => {
        let fname = build_test_path("sqrt", stringify!($name));
        run_test(fname.to_string(), 2, |case| {
            let (neg0, abytes) = case.get("a").unwrap();
            let (neg1, rbytes) = case.get("r").unwrap();
            assert!(!neg0 && !neg1);

            let a = $name::from_bytes(abytes);
            let r = $name::from_bytes(rbytes);
            assert_eq!(r, a.sqrt());
        });
    };
}