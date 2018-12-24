/// A trait definition for large numbers.
pub trait CryptoNum {
    /// Generate a new value of the given type.
    fn zero() -> Self;
    /// Test if the number is zero.
    fn is_zero(&self) -> bool;
    /// Test if the number is even.
    fn is_even(&self) -> bool;
    /// Test if the number is odd.
    fn is_odd(&self) -> bool;
    /// The size of this number in bits.
    fn bit_length() -> usize;
    /// Mask off the high parts of the number. In particular, it
    /// zeros the bits above (len * 64).
    fn mask(&mut self, len: usize);
    /// Test if the given bit is zero, where bits are numbered in
    /// least-significant order (0 is the LSB, etc.).
    fn testbit(&self, bit: usize) -> bool;
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

            fn bit_length() -> usize {
                $size * 64
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

            fn testbit(&self, bit: usize) -> bool {
                let idx = bit / 64;
                let offset = bit % 64;
                println!("Testing bit {} of {:X}", bit, self);
                if idx >= $size {
                    return false;
                }
                println!("{:x} & {:x} == {:x}", self.value[idx], 1u64 << offset, self.value[idx] & (1u64 << offset));
                (self.value[idx] & (1u64 << offset)) != 0
            }
        }

        #[cfg(test)]
        impl Arbitrary for $name {
            fn arbitrary<G: Gen>(g: &mut G) -> $name {
                let mut res = $name::zero();

                for val in res.value.iter_mut() {
                    *val = g.next_u64();
                }
                res
            }
        }

        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str(stringify!($name))?;
                f.write_str("{")?;
                f.debug_list().entries(self.value.iter()).finish()?;
                f.write_str("}")
            }
        }
    }
}

#[cfg(test)]
macro_rules! generate_base_tests
{
    ($name: ident, $lname: ident) => {
        #[test]
        fn $lname() {
            generate_base_tests!(body $name, $lname);
        }
    };
    (ignore $name: ident, $lname: ident) => {
        #[test]
        #[ignore]
        fn $lname() {
            generate_base_tests!(body $name, $lname);
        }
    };
    (body $name: ident, $lname: ident) => {
        let fname = build_test_path("base", stringify!($name));
        run_test(fname.to_string(), 8, |case| {
            let (neg0, xbytes) = case.get("x").unwrap();
            let (neg1, mbytes) = case.get("m").unwrap();
            let (neg2, zbytes) = case.get("z").unwrap();
            let (neg3, ebytes) = case.get("e").unwrap();
            let (neg4, obytes) = case.get("o").unwrap();
            let (neg5, rbytes) = case.get("r").unwrap();
            let (neg6, bbytes) = case.get("b").unwrap();
            let (neg7, tbytes) = case.get("t").unwrap();
            assert!(!neg0&&!neg1&&!neg2&&!neg3&&!neg4&&!neg5&&!neg6&&!neg7);
            let mut x = $name::from_bytes(xbytes);
            println!("---------");
            println!("x: {:x}", x);
            let m = $name::from_bytes(mbytes);
            let z = 1 == zbytes[0];
            let e = 1 == ebytes[0];
            let o = 1 == obytes[0];
            let r = $name::from_bytes(rbytes);
            let b = usize::from($name::from_bytes(bbytes));
            println!("b: {:x}", b);
            let t = 1 == tbytes[0];
            assert_eq!(x.is_zero(),  z);
            assert_eq!(x.is_even(),  e);
            assert_eq!(x.is_odd(),   o);
            assert_eq!(x.testbit(b), t);
            x.mask(usize::from(&m));
            assert_eq!(x, r);
        });
    }
}