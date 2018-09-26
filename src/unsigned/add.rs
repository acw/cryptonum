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

pub fn unsafe_addition(dest: &mut [u64], src: &[u64])
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
    assert_eq!(carry, 0, "Unsafe overflow in AddAssign");
}

macro_rules! addition_impls
{
    ($base: ident, $bigger: ident) => {
        impl AddAssign<$base> for $base {
            fn add_assign(&mut self, rhs: $base) {
                unsafe_addition(&mut self.value, &rhs.value);
            }
        }

        impl<'a> AddAssign<&'a $base> for $base {
            fn add_assign(&mut self, rhs: &$base) {
                unsafe_addition(&mut self.value, &rhs.value);
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

//addition_impls!(U192,   U256);