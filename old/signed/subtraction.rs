macro_rules! subtraction_impls
{
    ($name: ident, $bigger: ident, $ubigger: ident) => {
        impl SubAssign<$name> for $name {
            fn sub_assign(&mut self, rhs: $name) {
                self.sub_assign(&rhs);
            }
        }

        impl<'a> SubAssign<&'a $name> for $name {
            fn sub_assign(&mut self, rhs: &$name) {
                if self.negative == rhs.negative {
                    if &self.value >= &rhs.value {
                        self.value -= &rhs.value;
                    } else {
                        self.value = rhs.value.clone() - self.value.clone();
                        self.negative = !self.negative;
                    }
                } else {
                    unsafe_addition(&mut self.value.value, &rhs.value.value, true);
                }
            }
        }

        impl Sub<$name> for $name {
            type Output = $bigger;

            fn sub(self, rhs: $name) -> $bigger
            {
                &self - &rhs
            }
        }

        impl<'a> Sub<&'a $name> for $name {
            type Output = $bigger;

            fn sub(self, rhs: &$name) -> $bigger
            {
                &self - rhs
            }
        }

        impl<'a> Sub<$name> for &'a $name {
            type Output = $bigger;

            fn sub(self, rhs: $name) -> $bigger
            {
                self - &rhs
            }
        }

        impl<'a,'b> Sub<&'a $name> for &'b $name {
            type Output = $bigger;

            fn sub(self, rhs: &$name) -> $bigger
            {
                if self.negative == rhs.negative {
                    if &self.value >= &rhs.value {
                        $bigger {
                            negative: self.negative,
                            value: $ubigger::from(&self.value - &rhs.value)
                        }
                    } else {
                        $bigger {
                            negative: !self.negative,
                            value: $ubigger::from(&rhs.value - &self.value)
                        }
                    }
                } else {
                    $bigger {
                        negative: self.negative,
                        value: &self.value + &rhs.value
                    }
                }
            }
        }
    }
}

#[cfg(test)]
macro_rules! generate_sigsub_tests {
    ($sname: ident, $name: ident, $lname: ident, $bigger: ident, $ubigger: ident) => {
        #[test]
        fn $lname() {
            generate_sigsub_tests!(body$sname, $name, $lname, $bigger, $ubigger);
        }
    };
    (ignore $sname: ident, $name: ident, $lname: ident, $bigger: ident, $ubigger: ident) => {
        #[test]
        #[ignore]
        fn $lname() {
            generate_sigsub_tests!(body $sname, $name, $lname, $bigger, $ubigger);
        }
    };
    (body $sname: ident, $name: ident, $lname: ident, $bigger: ident, $ubigger: ident) => {
        let fname = build_test_path("sigsub", stringify!($sname));
        run_test(fname.to_string(), 3, |case| {
            let (nega, abytes) = case.get("a").unwrap();
            let (negb, bbytes) = case.get("b").unwrap();
            let (negc, cbytes) = case.get("c").unwrap();

            let mut a = $sname::new(*nega, $name::from_bytes(abytes));
            let     b = $sname::new(*negb, $name::from_bytes(bbytes));
            let     c = $bigger::new(*negc, $ubigger::from_bytes(cbytes));
            assert_eq!(c, &a - &b, "base subtraction");

            if c.value.value[c.value.value.len()-1] == 0 {
                a -= b;
                assert_eq!($sname::from(c), a, "in-place subtraction");
            }
        });
    };
}