use unsigned::add::unsafe_addition;

pub fn subtract(res: &mut [u64], spare: &mut [u64], other: &[u64])
{
    for i in 0..res.len() {
        res[i] = !res[i];
    }
    spare[0] = 1;
    unsafe_addition(res, &spare);
    unsafe_addition(res, &other);
}

macro_rules! subtraction_impls
{
    ($name: ident, $size: expr) => {
        impl SubAssign for $name {
            fn sub_assign(&mut self, rhs: $name) {
                let mut temp = [0; $size];
                subtract(&mut self.value, &mut temp, &rhs.value);
            }
        }

        impl<'a> SubAssign<&'a $name> for $name {
            fn sub_assign(&mut self, rhs: &$name) {
                let mut temp = [0; $size];
                subtract(&mut self.value, &mut temp, &rhs.value);
            }
        }

        impl Sub for $name {
            type Output = $name;

            fn sub(self, rhs: $name) -> $name {
                let mut res  = $name::zero();
                let mut temp = [0; $size];
                subtract(&mut res.value, &mut temp, &rhs.value);
                res
            }
        }

        impl<'a> Sub<$name> for &'a $name {
            type Output = $name;

            fn sub(self, rhs: $name) -> $name {
                let mut res  = $name::zero();
                let mut temp = [0; $size];
                subtract(&mut res.value, &mut temp, &rhs.value);
                res
            }
        }

        impl<'a> Sub<&'a $name> for $name {
            type Output = $name;

            fn sub(self, rhs: &$name) -> $name {
                let mut res  = $name::zero();
                let mut temp = [0; $size];
                subtract(&mut res.value, &mut temp, &rhs.value);
                res
            }
        }

        impl<'a,'b> Sub<&'a $name> for &'b $name {
            type Output = $name;

            fn sub(self, rhs: &$name) -> $name {
                let mut res  = $name::zero();
                let mut temp = [0; $size];
                subtract(&mut res.value, &mut temp, &rhs.value);
                res
            }
        }
    }
}

//subtraction_impls!(U192,   3);