pub trait CryptoNum {
    fn zero() -> Self;
    fn is_zero(&self) -> bool;
    fn is_even(&self) -> bool;
    fn is_odd(&self) -> bool;
    fn mask(&mut self, len: usize);
}

macro_rules! generate_base
{
    ($name: ident, $size: expr) => {
        #[derive(Clone)]
        pub struct $name {
            pub(crate) value: [u64; $size]
        }

        impl CryptoNum for $name {
            fn zero() -> $name {
                $name{ value: [0; $size] }
            }

            fn is_zero(&self) -> bool {
                self.value.iter().all(|&x| x == 0)
            }

            fn is_even(&self) -> bool {
                (self.value[0] & 0x1) == 0
            }

            fn is_odd(&self) -> bool {
                (self.value[0] & 0x1) == 1
            }

            fn mask(&mut self, len: usize) {
                let dellen = min(len, $size);
                for i in dellen..$size {
                    self.value[i] = 0;
                }
            }
        }
    }
}

#[cfg(test)]
macro_rules! generate_base_tests
{
    ($name: ident) => {
        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str(stringify!($name))?;
                f.write_str("{")?;
                f.debug_list().entries(self.value.iter()).finish()?;
                f.write_str("}")
            }
        }

        #[test]
        #[allow(non_snake_case)]
        fn $name() {
            let fname = format!("testdata/base/{}.tests", stringify!($name));
            run_test(fname.to_string(), 6, |case| {
                let (neg0, xbytes) = case.get("x").unwrap();
                let (neg1, mbytes) = case.get("m").unwrap();
                let (neg2, zbytes) = case.get("z").unwrap();
                let (neg3, ebytes) = case.get("e").unwrap();
                let (neg4, obytes) = case.get("o").unwrap();
                let (neg5, rbytes) = case.get("r").unwrap();
                assert!(!neg0 && !neg1 && !neg2 && !neg3 && !neg4 && !neg5);
                let mut x = $name::from_bytes(xbytes);
                let m = $name::from_bytes(mbytes);
                let z = 1 == zbytes[0];
                let e = 1 == ebytes[0];
                let o = 1 == obytes[0];
                let r = $name::from_bytes(rbytes);
                assert_eq!(x.is_zero(), z);
                assert_eq!(x.is_even(), e);
                assert_eq!(x.is_odd(),  o);
                x.mask(usize::from(&m));
                assert_eq!(x, r);
            });
        }
    }
}