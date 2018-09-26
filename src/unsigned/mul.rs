pub fn multiply(dest: &mut [u64], left: &[u64], right: &[u64])
{
    let len = right.len();

    assert_eq!(left.len(), len,   "Uneven argument lengths in multiply");
    assert_eq!(dest.len(), len*2, "Bad destination size in multiply");

    for i in 0..len {
        let mut carry = 0;

        for j in 0..len {
            let old = dest[i+j] as u128;
            let l128 = left[j] as u128;
            let r128 = right[i] as u128;
            let uv = old + (l128 * r128) + carry;
            dest[i+j] = uv as u64;
            carry = uv >> 64;
        }
        dest[i+len] = carry as u64;
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
    };
}