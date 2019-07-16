pub fn multiply(dest: &mut [u64], left: &[u64], right: &[u64])
{
    let len = right.len();
    let mut i = 0;

    assert_eq!(left.len(), len,   "Uneven argument lengths in multiply");
    assert_eq!(dest.len(), len*2, "Bad destination size in multiply");

    while i < len {
        let mut carry = 0;
        let mut j = 0;

        while j < len {
            let old = dest[i+j] as u128;
            let l128 = left[j] as u128;
            let r128 = right[i] as u128;
            let uv = old + (l128 * r128) + carry;
            dest[i+j] = uv as u64;
            carry = uv >> 64;
            j += 1;
        }
        dest[i+len] = carry as u64;
        i += 1;
    }
}

pub fn multiply_small(dest: &mut [u64], left: &[u64], right: &[u64])
{
    let len = right.len();

    assert_eq!(dest.len(), len);
    assert_eq!(left.len(), len);
    for i in 0..len { dest[i] = 0; }
    for i in 0..len {
        let mut carry = 0;

        for j in 0..len {
            if i+j >= len {
                carry = 0;
                continue;
            }
            let old = dest[i+j] as u128;
            let l128 = left[j] as u128;
            let r128 = right[i] as u128;
            let uv = old + (l128 * r128) + carry;
            dest[i+j] = uv as u64;
            carry = uv >> 64;
        }
    }
}

macro_rules! multiply_impls {
    ($name: ident, $dbl: ident) => {
        impl Mul<$name> for $name {
            type Output = $dbl;

            fn mul(self, rhs: $name) -> $dbl {
                let mut res = $dbl::zero();
                multiply(&mut res.value, &self.value, &rhs.value);
                res
            }
        }

        impl<'a> Mul<$name> for &'a $name {
            type Output = $dbl;

            fn mul(self, rhs: $name) -> $dbl {
                let mut res = $dbl::zero();
                multiply(&mut res.value, &self.value, &rhs.value);
                res
            }
        }

        impl<'a> Mul<&'a $name> for $name {
            type Output = $dbl;

            fn mul(self, rhs: &$name) -> $dbl {
                let mut res = $dbl::zero();
                multiply(&mut res.value, &self.value, &rhs.value);
                res
            }
        }

        impl<'a,'b> Mul<&'a $name> for &'b $name {
            type Output = $dbl;

            fn mul(self, rhs: &$name) -> $dbl {
                let mut res = $dbl::zero();
                multiply(&mut res.value, &self.value, &rhs.value);
                res
            }
        }

        impl MulAssign for $name {
            fn mul_assign(&mut self, rhs: $name) {
                let copy = self.value.clone();
                multiply_small(&mut self.value, &copy, &rhs.value);
            }
        }

        impl<'a> MulAssign<&'a $name> for $name {
            fn mul_assign(&mut self, rhs: &$name) {
                let copy = self.value.clone();
                multiply_small(&mut self.value, &copy, &rhs.value);
            }
        }
    };
}

#[cfg(test)]
macro_rules! generate_mul_tests
{
    ($name: ident, $lname: ident, $dbl: ident) => {
        #[test]
        fn $lname() {
            generate_mul_tests!(body $name, $lname, $dbl);
        }
    };
    (ignore $name: ident, $lname: ident, $dbl: ident) => {
        #[test]
        #[ignore]
        fn $lname() {
            generate_mul_tests!(body $name, $lname, $dbl);
        }
    };
    (body $name: ident, $lname: ident, $dbl: ident) => {
        let fname = build_test_path("mul", stringify!($name));
        run_test(fname.to_string(), 3, |case| {
            let (neg0, abytes) = case.get("a").unwrap();
            let (neg1, bbytes) = case.get("b").unwrap();
            let (neg2, cbytes) = case.get("c").unwrap();
            assert!(!neg0 && !neg1 && !neg2);

            let mut a = $name::from_bytes(abytes);
            let b = $name::from_bytes(bbytes);
            let c = $dbl::from_bytes(cbytes);
            assert_eq!(c, &a * &b, "standard multiplication");
            a *= b;
            assert_eq!($name::from(c), a);
        });
    };
}