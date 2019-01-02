pub fn scale(dest: &mut [u64], bignum: &[u64], factor: u64)
{
    let len = bignum.len();
    let factor128 = factor as u128;
    let mut carry = 0;

    assert_eq!(dest.len(), len+1, "Bad destination size in scale");
    for i in 0..len {
        let digit128 = bignum[i] as u128;
        let res128 = carry + (digit128 * factor128);
        dest[i] = res128 as u64;
        carry = res128 >> 64;
    }
    dest[len] = carry as u64;
}

macro_rules! scale_impls
{
    ($base: ident, $big: ident) => {
        scale_impls!($base, $big, u8);
        scale_impls!($base, $big, u16);
        scale_impls!($base, $big, u32);
        scale_impls!($base, $big, u64);
        scale_impls!($base, $big, usize);
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
                scale(&mut res.value, &self.value, factor as u64);
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
    }
}

#[cfg(test)]
macro_rules! generate_scale_tests
{
    ($name: ident, $lname: ident, $big: ident) => {
        #[test]
        fn $lname() {
            generate_scale_tests!(body $name, $lname, $big);
        }
    };
    (ignore $name: ident, $lname: ident, $big: ident) => {
        #[test]
        #[ignore]
        fn $lname() {
            generate_scale_tests!(body $name, $lname, $big);
        }
    };
    (body $name: ident, $lname: ident, $big: ident) => {
        let fname = build_test_path("scale", stringify!($name));
        run_test(fname.to_string(), 3, |case| {
            let (neg0, abytes) = case.get("a").unwrap();
            let (neg1, bbytes) = case.get("b").unwrap();
            let (neg2, cbytes) = case.get("c").unwrap();
            assert!(!neg0 && !neg1 && !neg2);

            let a = $name::from_bytes(abytes);
            let b = $name::from_bytes(bbytes);
            let c = $big::from_bytes(cbytes);
            assert_eq!(c, &a * b.value[0]);
        });
    };
}