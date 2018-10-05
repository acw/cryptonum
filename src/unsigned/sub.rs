use unsigned::add::unsafe_addition;

pub fn subtract(res: &mut [u64], spare: &mut [u64], other: &mut [u64])
{
    for i in 0..res.len() {
        other[i] = !other[i];
    }
    spare[0] = 1;
    unsafe_addition(other, &spare, true);
    unsafe_addition(res,   &other, true);
}

macro_rules! subtraction_impls
{
    ($name: ident, $size: expr) => {
        impl SubAssign for $name {
            fn sub_assign(&mut self, rhs: $name) {
                let mut temp = [0; $size];
                subtract(&mut self.value, &mut temp, &mut rhs.value.clone());
            }
        }

        impl<'a> SubAssign<&'a $name> for $name {
            fn sub_assign(&mut self, rhs: &$name) {
                let mut temp = [0; $size];
                subtract(&mut self.value, &mut temp, &mut rhs.value.clone());
            }
        }

        impl Sub for $name {
            type Output = $name;

            fn sub(self, rhs: $name) -> $name {
                let mut res  = self.clone();
                let mut temp = [0; $size];
                subtract(&mut res.value, &mut temp, &mut rhs.value.clone());
                res
            }
        }

        impl<'a> Sub<$name> for &'a $name {
            type Output = $name;

            fn sub(self, rhs: $name) -> $name {
                let mut res  = self.clone();
                let mut temp = [0; $size];
                subtract(&mut res.value, &mut temp, &mut rhs.value.clone());
                res
            }
        }

        impl<'a> Sub<&'a $name> for $name {
            type Output = $name;

            fn sub(self, rhs: &$name) -> $name {
                let mut res  = self.clone();
                let mut temp = [0; $size];
                subtract(&mut res.value, &mut temp, &mut rhs.value.clone());
                res
            }
        }

        impl<'a,'b> Sub<&'a $name> for &'b $name {
            type Output = $name;

            fn sub(self, rhs: &$name) -> $name {
                let mut res  = self.clone();
                let mut temp = [0; $size];
                subtract(&mut res.value, &mut temp, &mut rhs.value.clone());
                res
            }
        }
    }
}

#[cfg(test)]
macro_rules! generate_sub_tests {
    ($name: ident, $lname: ident) => {
        #[test]
        fn $lname() {
            let fname = format!("testdata/sub/{}.tests", stringify!($name));
            run_test(fname.to_string(), 3, |case| {
                let (neg0, abytes) = case.get("a").unwrap();
                let (neg1, bbytes) = case.get("b").unwrap();
                let (neg2, cbytes) = case.get("c").unwrap();
                assert!(!neg0 && !neg1 && !neg2);

                let mut a = $name::from_bytes(abytes);
                let b     = $name::from_bytes(bbytes);
                let c     = $name::from_bytes(cbytes);
                assert_eq!(c, &a - &b);
                a -= b;
                assert_eq!(c, a);
            });
        }
    };
}