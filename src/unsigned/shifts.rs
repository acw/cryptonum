pub(crate) fn shiftl(res: &mut [u64], copy: &[u64], amt: usize) {
    let digits = amt / 64;
    let bits   = amt % 64;

    let mut carry = 0;
    let     shift = 64 - bits;

    for i in 0..res.len() {
        let base = if i >= digits { copy[i-digits] } else { 0 };
        let new_carry = if shift == 64 { 0 } else { base >> shift };
        res[i] = (base << bits) | carry;
        carry = new_carry;
    }
}

pub(crate) fn shiftr(res: &mut [u64], copy: &[u64], amt: usize) {
    let digits = amt / 64;
    let bits   = amt % 64;

    let mut carry = 0;
    let mask = !(0xFFFFFFFFFFFFFFFF << bits);
    let shift = (64 - bits) as u32;

    for (idx, val) in res.iter_mut().enumerate().rev() {
        let target = idx + digits;
        let base = if target >= copy.len() { 0 } else { copy[target] };
        let (new_carry, _) = (base & mask).overflowing_shl(shift);
        *val = (base >> bits) | carry;
        carry = new_carry;
    }
}

macro_rules! shift_impls
{
    ($name: ident, $size: expr) => {
        impl ShlAssign<usize> for $name {
            fn shl_assign(&mut self, amt: usize) {
                let copy = self.value.clone();
                shiftl(&mut self.value, &copy, amt);
            }
        }

        impl Shl<usize> for $name {
            type Output = $name;

            fn shl(mut self, amt: usize) -> $name {
                let copy = self.value.clone();
                shiftl(&mut self.value, &copy, amt);
                self
            }
        }

        impl<'a> Shl<usize> for &'a $name {
            type Output = $name;

            fn shl(self, amt: usize) -> $name {
                let mut res = $name{ value: self.value.clone() };
                shiftl(&mut res.value, &self.value, amt);
                res
            }
        }

        impl ShrAssign<usize> for $name {
            fn shr_assign(&mut self, amt: usize) {
                let copy = self.value.clone();
                shiftr(&mut self.value, &copy, amt);
            }
        }

        impl Shr<usize> for $name {
            type Output = $name;

            fn shr(mut self, amt: usize) -> $name {
                let copy = self.value.clone();
                shiftr(&mut self.value, &copy, amt);
                self
            }
        }

        impl<'a> Shr<usize> for &'a $name {
            type Output = $name;

            fn shr(self, amt: usize) -> $name {
                let mut res = $name{ value: self.value.clone() };
                shiftr(&mut res.value, &self.value, amt);
                res
            }
        }
    }
}

#[cfg(test)]
macro_rules! generate_shiftl_tests {
    ($name: ident, $lname: ident) => {
        #[test]
        fn $lname() {
            let fname = format!("testdata/shiftl/{}.tests", stringify!($name));
            run_test(fname.to_string(), 3, |case| {
                let (neg0, abytes) = case.get("a").unwrap();
                let (neg1, lbytes) = case.get("l").unwrap();
                let (neg2, rbytes) = case.get("r").unwrap();
                assert!(!neg0 && !neg1 && !neg2);

                let a = $name::from_bytes(abytes);
                let l = $name::from_bytes(lbytes);
                let r = $name::from_bytes(rbytes);
                assert_eq!(r, a << usize::from(l));
            });
        }
    };
}

#[cfg(test)]
macro_rules! generate_shiftr_tests {
    ($name: ident, $lname: ident) => {
        #[test]
        fn $lname() {
            let fname = format!("testdata/shiftr/{}.tests", stringify!($name));
            run_test(fname.to_string(), 3, |case| {
                let (neg0, abytes) = case.get("a").unwrap();
                let (neg1, lbytes) = case.get("l").unwrap();
                let (neg2, rbytes) = case.get("r").unwrap();
                assert!(!neg0 && !neg1 && !neg2);

                let a = $name::from_bytes(abytes);
                let l = $name::from_bytes(lbytes);
                let r = $name::from_bytes(rbytes);
                assert_eq!(r, a >> usize::from(l));
            });
        }
    };
}