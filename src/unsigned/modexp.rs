pub trait ModExp<T> {
    fn modexp(&self, e: &Self, m: &T) -> Self;
}

macro_rules! modexp_impls {
    ($name: ident, $other: ident) => {
        impl ModExp<$other> for $name {
            fn modexp(&self, ine: &$name, m: &$other) -> $name {
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
}

#[cfg(test)]
macro_rules! generate_modexp_tests {
    ($name: ident, $lname: ident) => {
        #[test]
        fn $lname() {
            generate_modexp_tests!(body $name, $lname);
        }
    };
    (ignore $name: ident, $lname: ident) => {
        #[test]
        #[ignore]
        fn $lname() {
            generate_modexp_tests!(body $name, $lname);
        }
    };
    (body $name: ident, $lname: ident) => {
        let fname = format!("testdata/modexp/{}.tests", stringify!($name));
        run_test(fname.to_string(), 6, |case| {
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
    };
}

#[cfg(test)]
macro_rules! generate_barrett_modexp_tests {
    (ignore $name: ident, $lname: ident, $bname: ident) => {
        #[test]
        #[ignore]
        fn $lname() {
            generate_barrett_modexp_tests!(body $name, $lname, $bname);
        }
    };
    ($name: ident, $lname: ident, $bname: ident) => {
        #[test]
        fn $lname() {
            generate_barrett_modexp_tests!(body $name, $lname, $bname);
        }
    };
    (body $name: ident, $lname: ident, $bname: ident) => {
        let fname = format!("testdata/modexp/{}.tests", stringify!($name));
        run_test(fname.to_string(), 6, |case| {
            let (neg0, bbytes) = case.get("b").unwrap();
            let (neg1, ebytes) = case.get("e").unwrap();
            let (neg2, mbytes) = case.get("m").unwrap();
            let (neg3, rbytes) = case.get("r").unwrap();
            let (neg4, kbytes) = case.get("k").unwrap();
            let (neg5, ubytes) = case.get("u").unwrap();
            assert!(!neg0 && !neg1 && !neg2 && !neg3 && !neg4 && !neg5);

            let b    = $name::from_bytes(bbytes);
            let e    = $name::from_bytes(ebytes);
            let m    = $name::from_bytes(mbytes);
            let r    = $name::from_bytes(rbytes);
            let kbig = $name::from_bytes(kbytes);
            let k    = usize::from(kbig);
            let mu   = $bname::from_bytes(ubytes);
            let bar  = $name::new_barrett(k, $bname::from(m), mu);

            if k == b.value.len() {
                assert_eq!(r, b.modexp(&e, &bar));
            }
        });
    };
}