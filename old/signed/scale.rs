macro_rules! scale_impls
{
    ($base: ident, $big: ident) => {
        scale_impls!($base, $big, u8);
        scale_impls!($base, $big, u16);
        scale_impls!($base, $big, u32);
        scale_impls!($base, $big, u64);
        scale_impls!($base, $big, usize);

        scale_impls!($base, $big, scaled i8);
        scale_impls!($base, $big, scaled i16);
        scale_impls!($base, $big, scaled i32);
        scale_impls!($base, $big, scaled i64);
        scale_impls!($base, $big, scaled isize);
    };
    ($base: ident, $big: ident, $prim: ident) => {
        impl Mul<$prim> for $base {
            type Output = $big;

            fn mul(self, factor: $prim) -> $big {
                &self * factor
            }
        }

        impl<'a> Mul<$prim> for &'a $base {
            type Output = $big;

            fn mul(self, factor: $prim) -> $big {
                let mut res = $big::zero();
                scale(&mut res.value.value, &self.value.value, factor as u64);
                res.negative = self.negative && !res.value.is_zero();
                res
            }
        }

        impl Mul<$base> for $prim {
            type Output = $big;

            fn mul(self, rhs: $base) -> $big {
                rhs.mul(self)
            }
        }

        impl<'a> Mul<&'a $base> for $prim {
            type Output = $big;

            fn mul(self, rhs: &$base) -> $big {
                rhs.mul(self)
            }
        }
    };
    ($base: ident, $big: ident, scaled $prim: ident) => {
        impl Mul<$prim> for $base {
            type Output = $big;

            fn mul(self, factor: $prim) -> $big {
                &self * factor
            }
        }

        impl<'a> Mul<$prim> for &'a $base {
            type Output = $big;

            fn mul(self, factor: $prim) -> $big {
                let mut res = $big::zero();
                scale(&mut res.value.value, &self.value.value, factor.abs() as u64);
                res.negative = (self.negative ^ (factor < 0)) && !res.value.is_zero();
                res
            }
        }

        impl Mul<$base> for $prim {
            type Output = $big;

            fn mul(self, rhs: $base) -> $big {
                rhs.mul(self)
            }
        }

        impl<'a> Mul<&'a $base> for $prim {
            type Output = $big;

            fn mul(self, rhs: &$base) -> $big {
                rhs.mul(self)
            }
        }
    };
}

#[cfg(test)]
macro_rules! generate_sigscale_tests
{
    ($name: ident, $uname: ident, $lname: ident, $big: ident, $ubig: ident) => {
        #[test]
        fn $lname() {
            generate_sigscale_tests!(body $name, $uname, $lname, $big, $ubig);
        }
    };
    (ignore $name: ident, $uname:ident, $lname: ident, $big: ident, $ubig: ident) => {
        #[test]
        #[ignore]
        fn $lname() {
            generate_sigscale_tests!(body $name, $uname, $lname, $big, $ubig);
        }
    };
    (body $name: ident, $uname: ident, $lname: ident, $big: ident, $ubig: ident) => {
        let fname = build_test_path("sigscale", stringify!($name));
        run_test(fname.to_string(), 3, |case| {
            let (nega, abytes) = case.get("a").unwrap();
            let (negb, bbytes) = case.get("b").unwrap();
            let (negc, cbytes) = case.get("c").unwrap();

            let a    = $name::new(*nega, $uname::from_bytes(abytes));
            let bbig = $name::new(*negb, $uname::from_bytes(bbytes));
            let c    = $big::new(*negc, $ubig::from_bytes(cbytes));
            let b    = i64::from(&bbig);
            let res = &a * b;
            assert_eq!(c, res);
        });
    };
}