pub trait ModExp<T> {
    fn modexp(&self, e: &Self, m: &T) -> Self;
}

macro_rules! modexp_impls {
    ($name: ident) => {
//        impl ModExp<$name> for $name {
//            fn modexp(&self, e: &$name, m: &$name) -> $name {
//                // S <- g
//                let mut s = self.clone();
//                // A <- 1
//                let mut a = $name::from(1u64);
//                // We do a quick skim through and find the highest index that
//                // actually has a value in it.
//                let mut e = ine.clone();
//                // While e != 0 do the following:
//                while e.values.iter().any(|x| *x != 0) {
//                    // If e is odd then A <- A * S
//                    if e.values[0] & 1 != 0 {
//                        a.modmul(&s, m);
//                    }
//                    // e <- floor(e / 2)
//                    let mut carry = 0;
//                    e.values.iter_mut().rev().for_each(|x| {
//                        let new_carry = *x & 1;
//                        *x = (*x >> 1) | (carry << 63);
//                        carry = new_carry;
//                    });
//                    // If e != 0 then S <- S * S
//                    s.modsq(m);
//                }
//                // Return A
//                a
//            }
//        }
    };
    ($name: ident, $barrett: ident) => {
    };
}