macro_rules! add_impls
{
    ($name: ident, $bigger: ident, $ubigger: ident) => {
        impl AddAssign<$name> for $name {
            fn add_assign(&mut self, rhs: $name) {
                self.add_assign(&rhs);
            }
        }

        impl<'a> AddAssign<&'a $name> for $name {
            fn add_assign(&mut self, rhs: &$name) {
                if self.negative == rhs.negative {
                    unsafe_addition(&mut self.value.value, &rhs.value.value, true);
                } else {
                    if self.value > rhs.value {
                        self.value -= rhs.value.clone();
                    } else {
                        self.value = rhs.value.clone() - self.value.clone();
                        self.negative = rhs.negative;
                    }
                }
            }
        }

        impl Add<$name> for $name {
            type Output = $bigger;

            fn add(self, rhs: $name) -> $bigger
            {
                &self + &rhs
            }
        }

        impl<'a> Add<&'a $name> for $name {
            type Output = $bigger;

            fn add(self, rhs: &$name) -> $bigger
            {
                &self + rhs
            }
        }

        impl<'a> Add<$name> for &'a $name {
            type Output = $bigger;

            fn add(self, rhs: $name) -> $bigger
            {
                self + &rhs
            }
        }

        impl<'a,'b> Add<&'a $name> for &'b $name {
            type Output = $bigger;

            fn add(self, rhs: &$name) -> $bigger
            {
                if self.negative == rhs.negative {
                    $bigger{
                        negative: self.negative, 
                        value: &self.value + &rhs.value
                    }
                } else {
                    match self.value.cmp(&rhs.value) {
                        Ordering::Greater =>
                            $bigger {
                                negative: self.negative,
                                value: $ubigger::from(&self.value - &rhs.value)
                            },
                        Ordering::Less =>
                            $bigger {
                                negative: rhs.negative,
                                value: $ubigger::from(&rhs.value - &self.value)
                            },
                        Ordering::Equal =>
                            $bigger::zero()
                    }
                }
            }
        }
    }
}

#[cfg(test)]
macro_rules! generate_sigadd_tests {
    ($sname: ident, $name: ident, $lname: ident, $bigger: ident, $ubigger: ident) => {
        #[test]
        fn $lname() {
            generate_sigadd_tests!(body$sname, $name, $lname, $bigger, $ubigger);
        }
    };
    (ignore $sname: ident, $name: ident, $lname: ident, $bigger: ident, $ubigger: ident) => {
        #[test]
        #[ignore]
        fn $lname() {
            generate_sigadd_tests!(body $sname, $name, $lname, $bigger, $ubigger);
        }
    };
    (body $sname: ident, $name: ident, $lname: ident, $bigger: ident, $ubigger: ident) => {
        let fname = build_test_path("sigadd", stringify!($name));
        run_test(fname.to_string(), 3, |case| {
            let (nega, abytes) = case.get("a").unwrap();
            let (negb, bbytes) = case.get("b").unwrap();
            let (negc, cbytes) = case.get("c").unwrap();

            let mut a = $sname::new(*nega, $name::from_bytes(abytes));
            let     b = $sname::new(*negb, $name::from_bytes(bbytes));
            let     c = $bigger::new(*negc, $ubigger::from_bytes(cbytes));
            assert_eq!(c, &a + &b, "base addition");

            if c.value.value[c.value.value.len()-1] == 0 {
                a += b;
                assert_eq!($sname::from(c), a, "mutating addition");
            }
        });
    };
}