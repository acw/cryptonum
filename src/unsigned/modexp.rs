pub trait ModExp<T> {
    fn modexp(&self, e: &Self, m: &T) -> Self;
}

macro_rules! modexp_impls {
    ($name: ident) => {
        impl ModExp<$name> for $name {
            fn modexp(&self, ine: &$name, m: &$name) -> $name {
                // S <- g
                let mut s = self.clone();
                // A <- 1
                let mut a = $name::from(1u64);
                // We do a quick skim through and find the highest index that
                // actually has a value in it.
                let mut e = ine.clone();
                // While e != 0 do the following:
                while e.value.iter().any(|x| *x != 0) {
                    // If e is odd then A <- A * S
                    if e.value[0] & 1 != 0 {
                        a = a.modmul(&s, m);
                        println!("Updating a to {:X}", a);
                    }
                    // e <- floor(e / 2)
                    let mut carry = 0;
                    e.value.iter_mut().rev().for_each(|x| {
                        let new_carry = *x & 1;
                        *x = (*x >> 1) | (carry << 63);
                        carry = new_carry;
                    });
                    // If e != 0 then S <- S * S
                    s = s.modsq(m);
                }
                // Return A
                a
            }
        }
    };
    ($name: ident, $barrett: ident) => {
    };
}

#[cfg(test)]
macro_rules! generate_modexp_tests {
    ($name: ident, $lname: ident) => {
        #[test]
        fn $lname() {
            let fname = format!("testdata/modexp/{}.tests", stringify!($name));
            run_test(fname.to_string(), 4, |case| {
                let (neg0, bbytes) = case.get("b").unwrap();
                let (neg1, ebytes) = case.get("e").unwrap();
                let (neg2, mbytes) = case.get("m").unwrap();
                let (neg3, rbytes) = case.get("r").unwrap();
                assert!(!neg0 && !neg1 && !neg2 && !neg3);

                let b = $name::from_bytes(bbytes);
                let e = $name::from_bytes(ebytes);
                let m = $name::from_bytes(mbytes);
                let r = $name::from_bytes(rbytes);
                assert_eq!(r, b.modexp(&e, &m));
            });
        }
    };
}