macro_rules! signed_impls {
    ($sname: ident, $name: ident) => {
        #[derive(Clone, PartialEq, Eq)]
        pub struct $sname {
            negative: bool,
            value: $name
        }

        impl $sname {
            pub fn new(negative: bool, value: $name) -> $sname {
                if value.is_zero() {
                    $sname{ negative: false, value: value }
                } else {
                $sname{ negative: negative, value: value }
            }
            }
 
            pub fn negate(&self) -> $sname {
                if self.value.is_zero() {
                    self.clone()
                } else {
                $sname{ negative: !self.negative, value: self.value.clone() }
            }
            }
 
            pub fn abs(&self) -> $sname {
                $sname{ negative: false, value: self.value.clone() }
            }

            pub fn is_negative(&self) -> bool {
                self.negative
            }
        }
 
        impl From<$sname> for $name {
            fn from(x: $sname) -> $name {
                x.value
            }
        }
 
        impl<'a> From<&'a $sname> for $name {
            fn from(x: &$sname) -> $name {
                x.value.clone()
            }
        }
 
        impl From<$name> for $sname {
            fn from(x: $name) -> $sname {
                $sname{ negative: false, value: x }
            }
        }
 
        impl<'a> From<&'a $name> for $sname {
            fn from(x: &$name) -> $sname {
                $sname{ negative: false, value: x.clone() }
            }
        }
 
        impl CryptoNum for $sname {
            fn zero() -> $sname {
                $sname{ negative: false, value: $name::zero() }
            }
 
            fn is_zero(&self) -> bool {
                self.value.is_zero()
            }
 
            fn is_even(&self) -> bool {
                self.value.is_even()
            }
 
            fn is_odd(&self) -> bool {
                self.value.is_odd()
            }
 
            fn mask(&mut self, len: usize) {
                self.value.mask(len);
            }
        }

//        #[cfg(test)]
//        impl Arbitrary for $sname {
//            fn arbitrary<G: Gen>(g: &mut G) -> $name {
//                let neg = g.gen::<bool>();
//                let val = g.gen::<$name>();
//                $sname{ negative: neg, value: val }
//            }
//        }

        #[cfg(test)]
        impl fmt::Debug for $sname {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                if self.negative {
                    f.write_str("-")?;
                }
                self.value.fmt(f)
            }
        }

        impl fmt::UpperHex for $sname {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                if self.negative {
                    write!(f, "-{:X}", self.value)
                } else {
                    if f.sign_plus() {
                        write!(f, "+{:X}", self.value)
                    } else {
                        write!(f, "{:X}", self.value)
                    }
                }
            }
        }

        impl fmt::LowerHex for $sname {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                if self.negative {
                    write!(f, "-{:x}", self.value)
                } else {
                    if f.sign_plus() {
                        write!(f, "+{:x}", self.value)
                    } else {
                        write!(f, "{:x}", self.value)
                    }
                }
            }
        }

        generate_base_conversions!($sname, $name);
    };
}

#[cfg(test)]
macro_rules! generate_signed_tests
{
    ($sname: ident, $name: ident, $lname: ident) => {
        #[test]
        fn $lname() {
            generate_signed_tests!(body $sname, $name, $lname);
        }
    };
    (ignore $sname: ident, $name: ident, $lname: ident) => {
        #[test]
        #[ignore]
        fn $lname() {
            generate_signed_tests!(body $sname, $name, $lname);
        }
    };
    (body $sname: ident, $name: ident, $lname: ident) => {
        let fname = format!("testdata/signed/{}.tests", stringify!($sname));
        run_test(fname.to_string(), 4, |case| {
            let (negx, xbytes) = case.get("x").unwrap();
            let (neg0, zbytes) = case.get("z").unwrap();
            let (neg1, ebytes) = case.get("e").unwrap();
            let (neg2, obytes) = case.get("o").unwrap();

            assert!(!neg0 && !neg1 && !neg2); 
            let x = $sname::new(*negx, $name::from_bytes(xbytes));
            let z = 1 == zbytes[0];
            let e = 1 == ebytes[0];
            let o = 1 == obytes[0];
            assert_eq!(x.is_zero(), z);
            assert_eq!(x.is_even(), e);
            assert_eq!(x.is_odd(),  o);
        });
    }
}