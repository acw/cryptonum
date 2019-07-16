macro_rules! div_impls
{
    ($name: ident, $uname: ident) => {
        impl DivAssign<$name> for $name {
            fn div_assign(&mut self, rhs: $name) {
                self.div_assign(&rhs);
            }
        }

        impl<'a> DivAssign<&'a $name> for $name {
            fn div_assign(&mut self, rhs: &$name) {
                let (q,r) = self.value.divmod(&rhs.value);
                if self.negative ^ rhs.negative {
                    self.value.value.copy_from_slice(&q.value);
                    if !r.is_zero() {
                        self.value += $uname::from(1u64);
                    }
                    self.negative = !self.value.is_zero();
                } else {
                    self.value.value.copy_from_slice(&q.value);
                    self.negative = false;
                }
            }
        }

        impl Div<$name> for $name {
            type Output = $name;

            fn div(self, rhs: $name) -> $name
            {
                &self / &rhs
            }
        }

        impl<'a> Div<&'a $name> for $name {
            type Output = $name;

            fn div(self, rhs: &$name) -> $name
            {
                &self / rhs
            }
        }

        impl<'a> Div<$name> for &'a $name {
            type Output = $name;

            fn div(self, rhs: $name) -> $name
            {
                self / &rhs
            }
        }

        impl<'a,'b> Div<&'a $name> for &'b $name {
            type Output = $name;

            fn div(self, rhs: &$name) -> $name
            {
                let mut outval = self.clone();
                outval /= rhs;
                outval
            }
        }

        impl RemAssign<$name> for $name {
            fn rem_assign(&mut self, rhs: $name) {
                self.rem_assign(&rhs);
            }
        }

        impl<'a> RemAssign<&'a $name> for $name {
            fn rem_assign(&mut self, rhs: &$name) {
                if rhs.negative != self.negative {
                    self.negative = !self.negative;
                    let modres = &self.value % &rhs.value;
                    self.value.value.copy_from_slice(&rhs.value.value);
                    self.value -= modres;
                } else {
                    self.value %= &rhs.value;
                }
            }
        }

        impl Rem<$name> for $name {
            type Output = $name;

            fn rem(self, rhs: $name) -> $name
            {
                &self % &rhs
            }
        }

        impl<'a> Rem<&'a $name> for $name {
            type Output = $name;

            fn rem(self, rhs: &$name) -> $name
            {
                &self % rhs
            }
        }

        impl<'a> Rem<$name> for &'a $name {
            type Output = $name;

            fn rem(self, rhs: $name) -> $name
            {
                self % &rhs
            }
        }

        impl<'a,'b> Rem<&'a $name> for &'b $name {
            type Output = $name;

            fn rem(self, rhs: &$name) -> $name
            {
                let mut outval = self.clone();
                outval %= rhs;
                outval
            }
        }
     }
}

#[cfg(test)]
macro_rules! generate_sigdiv_tests {
    ($sname: ident, $name: ident, $lname: ident) => {
        #[test]
        fn $lname() {
            generate_sigdiv_tests!(body $sname, $name);
        }
    };
    (ignore $sname: ident, $name: ident, $lname: ident) => {
        #[test]
        #[ignore]
        fn $lname() {
            generate_sigdiv_tests!(body $sname, $name);
        }
    };
    (body $sname: ident, $name: ident) => {
        let fname = build_test_path("sigdiv", stringify!($sname));
        run_test(fname.to_string(), 4, |case| {
            let (nega, abytes) = case.get("a").unwrap();
            let (negb, bbytes) = case.get("b").unwrap();
            let (negq, qbytes) = case.get("q").unwrap();
            let (negr, rbytes) = case.get("r").unwrap();

            let a = $sname::new(*nega, $name::from_bytes(abytes));
            let b = $sname::new(*negb, $name::from_bytes(bbytes));
            let q = $sname::new(*negq, $name::from_bytes(qbytes));
            let r = $sname::new(*negr, $name::from_bytes(rbytes));
            let d = &a / &b;
            let m = &a % &b;
            assert_eq!(q, d, "base div");
            assert_eq!(r, m, "base mod");
        });
    };
}