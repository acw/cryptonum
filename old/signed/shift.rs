
macro_rules! shift_impls
{
    ($sname: ident, $name: ident) => {
        impl ShlAssign<usize> for $sname {
            fn shl_assign(&mut self, amt: usize) {
                self.value <<= amt;
                if self.value.is_zero() {
                    self.negative = false;
                }
            }
        }

        impl Shl<usize> for $sname {
            type Output = $sname;

            fn shl(mut self, amt: usize) -> $sname {
                self <<= amt;
                self
            }
        }

        impl<'a> Shl<usize> for &'a $sname {
            type Output = $sname;

            fn shl(self, amt: usize) -> $sname {
                let mut res = self.clone();
                res <<= amt;
                res
            }
        }

        impl ShrAssign<usize> for $sname {
            fn shr_assign(&mut self, amt: usize) {
                // arithmatic right shift is normal right shift, but always rounding
                // to negative infinity. To implement this, we first shift right by
                // rhs bits, and then shift that value back left rhs bits. If the two
                // are the same, we just cleared out even bits, and there's no rounding
                // to worry about. If they aren't the same, then we add one back.
                let original = self.value.clone();
                self.value >>= amt;
                if self.negative {
                    let review = self.value.clone() << amt;
                    if review != original {
                        self.value += $name::from(1u64);
                    }
                }
                if self.value.is_zero() {
                    self.negative = false;
                }
            }
        }

        impl Shr<usize> for $sname {
            type Output = $sname;

            fn shr(mut self, amt: usize) -> $sname {
                self >>= amt;
                self
            }
        }

        impl<'a> Shr<usize> for &'a $sname {
            type Output = $sname;

            fn shr(self, amt: usize) -> $sname {
                let mut res = self.clone();
                res >>= amt;
                res
            }
        }
    }
}

#[cfg(test)]
macro_rules! generate_sigshiftl_tests {
    ($sname: ident, $name: ident, $lname: ident) => {
        #[test]
        fn $lname() {
            generate_sigshiftl_tests!(body $sname, $name, $lname);
        }
    };
    (ignore $sname: ident, $name: ident, $lname: ident) => {
        #[test]
        #[ignore]
        fn $lname() {
            generate_sigshiftl_tests!(body $sname, $name, $lname);
        }
    };
    (body $sname: ident, $name: ident, $lname: ident) => {
        let fname = build_test_path("sigshiftl", stringify!($sname));
        run_test(fname.to_string(), 3, |case| {
            let (nega, abytes) = case.get("a").unwrap();
            let (neg1, lbytes) = case.get("l").unwrap();
            let (negr, rbytes) = case.get("r").unwrap();
            assert!(!neg1);

            let a = $sname::new(*nega, $name::from_bytes(abytes));
            let l = $name::from_bytes(lbytes);
            let r = $sname::new(*negr, $name::from_bytes(rbytes));
            assert_eq!(r, a << usize::from(l));
        });
    };
}

#[cfg(test)]
macro_rules! generate_sigshiftr_tests {
    ($sname: ident, $name: ident, $lname: ident) => {
        #[test]
        fn $lname() {
            generate_sigshiftr_tests!(body $sname, $name, $lname);
        }
    };
    (ignore $sname: ident, $name: ident, $lname: ident) => {
        #[test]
        #[ignore]
        fn $lname() {
            generate_sigshiftr_tests!(body $sname, $name, $lname);
        }
    };
    (body $sname: ident, $name: ident, $lname: ident) => {
        let fname = build_test_path("sigshiftr", stringify!($sname));
        run_test(fname.to_string(), 3, |case| {
            let (nega, abytes) = case.get("a").unwrap();
            let (neg1, lbytes) = case.get("l").unwrap();
            let (negr, rbytes) = case.get("r").unwrap();
            assert!(!neg1);

            let a = $sname::new(*nega, $name::from_bytes(abytes));
            let l = $name::from_bytes(lbytes);
            let r = $sname::new(*negr, $name::from_bytes(rbytes));
            assert_eq!(r, a >> usize::from(l));
        });
    };
}