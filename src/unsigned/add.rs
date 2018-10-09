pub fn addition(dest: &mut [u64], src: &[u64])
{
    assert_eq!(dest.len(), src.len() + 1);
    let mut carry: u128 = 0;

    for i in 0..src.len() {
        let x128 = dest[i] as u128;
        let y128 = src[i] as u128;
        let z128 = x128 + y128 + carry;

        dest[i] = z128 as u64;
        carry = z128 >> 64;
    }
    dest[src.len()] = carry as u64;
}

pub fn unsafe_addition(dest: &mut [u64], src: &[u64], really_unsafe: bool)
{
    assert_eq!(dest.len(), src.len());
    let mut carry: u128 = 0;

    for i in 0..src.len() {
        let x128 = dest[i] as u128;
        let y128 = src[i] as u128;
        let z128 = x128 + y128 + carry;

        dest[i] = z128 as u64;
        carry = z128 >> 64;
    }

    if !really_unsafe {
        assert_eq!(carry, 0, "Unsafe overflow in AddAssign");
    }
}

macro_rules! addition_impls
{
    ($base: ident, $bigger: ident) => {
        impl AddAssign<$base> for $base {
            fn add_assign(&mut self, rhs: $base) {
                unsafe_addition(&mut self.value, &rhs.value, false);
            }
        }

        impl<'a> AddAssign<&'a $base> for $base {
            fn add_assign(&mut self, rhs: &$base) {
                unsafe_addition(&mut self.value, &rhs.value, false);
            }
        }

        impl Add<$base> for $base {
            type Output = $bigger;

            fn add(self, rhs: $base) -> $bigger
            {
                let mut dest = $bigger::zero();
                &dest.value[0..rhs.value.len()].copy_from_slice(&self.value);
                addition(&mut dest.value, &rhs.value);
                dest
            }
        }

        impl<'a> Add<&'a $base> for $base {
            type Output = $bigger;

            fn add(self, rhs: &$base) -> $bigger
            {
                let mut dest = $bigger::zero();
                &dest.value[0..rhs.value.len()].copy_from_slice(&self.value);
                addition(&mut dest.value, &rhs.value);
                dest
            }
        }

        impl<'a> Add<$base> for &'a $base {
            type Output = $bigger;

            fn add(self, rhs: $base) -> $bigger
            {
                let mut dest = $bigger::zero();
                &dest.value[0..rhs.value.len()].copy_from_slice(&self.value);
                addition(&mut dest.value, &rhs.value);
                dest
            }
        }

        impl<'a,'b> Add<&'a $base> for &'b $base {
            type Output = $bigger;

            fn add(self, rhs: &$base) -> $bigger
            {
                let mut dest = $bigger::zero();
                &dest.value[0..rhs.value.len()].copy_from_slice(&self.value);
                addition(&mut dest.value, &rhs.value);
                dest
            }
        }
    }
}

#[cfg(test)]
macro_rules! generate_add_tests {
    ($name: ident, $lname: ident, $plus1: ident) => {
        #[test]
        fn $lname() {
            generate_add_tests!(body $name, $lname, $plus1);
        }
    };
    (ignore $name: ident, $lname: ident, $plus1: ident) => {
        #[test]
        #[ignore]
        fn $lname() {
            generate_add_tests!(body $name, $lname, $plus1);
        }
    };
    (body $name: ident, $lname: ident, $plus1: ident) => {
        let fname = format!("testdata/add/{}.tests", stringify!($name));
        run_test(fname.to_string(), 3, |case| {
            let (neg0, abytes) = case.get("a").unwrap();
            let (neg1, bbytes) = case.get("b").unwrap();
            let (neg2, cbytes) = case.get("c").unwrap();
            assert!(!neg0 && !neg1 && !neg2);

            let a = $name::from_bytes(abytes);
            let b = $name::from_bytes(bbytes);
            let c = $plus1::from_bytes(cbytes);
            assert_eq!(c, &a + &b);

            if c.value[c.value.len()-1] == 0 {
                let mut aprime = a.clone();
                aprime += b;
                assert_eq!($name::from(c), aprime);
            }
        });
    };
}