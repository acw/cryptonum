macro_rules! mul_impls
{
    ($name: ident, $dbl: ident) => {
        impl MulAssign<$name> for $name {
            fn mul_assign(&mut self, rhs: $name) {
                self.mul_assign(&rhs);
            }
        }

        impl<'a> MulAssign<&'a $name> for $name {
            fn mul_assign(&mut self, rhs: &$name) {
                self.value *= &rhs.value;
                self.negative = !self.value.is_zero() && (self.negative != rhs.negative);
            }
        }

        impl Mul<$name> for $name {
            type Output = $dbl;

            fn mul(self, rhs: $name) -> $dbl
            {
                &self * &rhs
            }
        }

        impl<'a> Mul<&'a $name> for $name {
            type Output = $dbl;

            fn mul(self, rhs: &$name) -> $dbl
            {
                &self * rhs
            }
        }

        impl<'a> Mul<$name> for &'a $name {
            type Output = $dbl;

            fn mul(self, rhs: $name) -> $dbl
            {
                self * &rhs
            }
        }

        impl<'a,'b> Mul<&'a $name> for &'b $name {
            type Output = $dbl;

            fn mul(self, rhs: &$name) -> $dbl
            {
                let outval = &self.value * &rhs.value;
                let zero = outval.is_zero();
                $dbl {
                    value: outval,
                    negative: !zero && (self.negative != rhs.negative)
                }
            }
        }

        impl Square<$dbl> for $name {
            fn square(&self) -> $dbl {
                $dbl{ negative: false, value: self.value.square() }
            }
        }
    }
}

#[cfg(test)]
macro_rules! generate_sigmul_tests {
    ($sname: ident, $name: ident, $lname: ident, $dbl: ident, $udbl: ident) => {
        #[test]
        fn $lname() {
            generate_sigmul_tests!(body $sname, $name, $dbl, $udbl);
        }
    };
    (ignore $sname: ident, $name: ident, $lname: ident, $dbl: ident, $udbl: ident) => {
        #[test]
        #[ignore]
        fn $lname() {
            generate_sigmul_tests!(body $sname, $name, $dbl, $udbl);
        }
    };
    (body $sname: ident, $name: ident, $dbl: ident, $udbl: ident) => {
        let fname = build_test_path("sigmul", stringify!($sname));
        run_test(fname.to_string(), 3, |case| {
            let (nega, abytes) = case.get("a").unwrap();
            let (negb, bbytes) = case.get("b").unwrap();
            let (negc, cbytes) = case.get("c").unwrap();

            let mut a = $sname::new(*nega, $name::from_bytes(abytes));
            let     b = $sname::new(*negb, $name::from_bytes(bbytes));
            let     c = $dbl::new(*negc, $udbl::from_bytes(cbytes));
            assert_eq!(c, &a * &b, "base mul");
            a *= b;
            assert_eq!($sname::from(c), a, "in-place mul");
        });
    };
}