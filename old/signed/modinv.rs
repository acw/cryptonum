/// Computations of the modular inverse.
pub trait ModInv: Sized {
    /// Compute the modular inverse of this number under the given
    /// modulus, if it exists. If self is a, the modulus / argument
    /// is phi, and the result is Some(m), then (a * m) % phi = 1.
    fn modinv(&self, phi: &Self) -> Option<Self>;
}

macro_rules! smodinv_impls {
    ($name: ident, $bigger: ident) => {
        impl ModInv for $name {
            fn modinv(&self, phi: &$name) -> Option<$name>
            {
                let (_, mut b, g) = phi.egcd(&self);

                if g != $bigger::from(1u64) {
                    return None;
                }

                let bigphi = $bigger::from(phi);

                while b.is_negative() {
                    b += &bigphi;
                }

                if b > bigphi {
                    b -= &bigphi;
                }

                Some($name::from(b))
            }
        }
    }
}

macro_rules! modinv_impls {
    ($name: ident, $sname: ident, $uname: ident) => {
        impl ModInv for $name {
            fn modinv(&self, phi: &$name) -> Option<$name>
            {
                let (_, mut b, g) = phi.egcd(&self);

                if g != $sname::from(1i64) {
                    return None;
                }

                let sphi = $sname::from($uname::from(phi));

                while b.is_negative() {
                    b += &sphi;
                }

                if b > sphi {
                    b -= &sphi;
                }

                Some($name::from($uname::from(b)))
            }
        }
    };
}

#[cfg(test)]
macro_rules! generate_modinv_tests {
    ($sname: ident, $tname: ident, $mname: ident) => {
        #[test]
        fn $mname() {
            generate_modinv_tests!(body $sname, $tname, $mname);
        }
    };
    (ignore $sname: ident, $tname: ident, $mname: ident) => {
        #[test]
        #[ignore]
        fn $mname() {
            generate_modinv_tests!(body $sname, $tname, $mname);
        }
    };
    (body $sname: ident, $tname: ident, $mname: ident) => {
        let fname = build_test_path("modinv", stringify!($sname));
        run_test(fname.to_string(), 3, |case| {
            let (nega, abytes) = case.get("a").unwrap();
            let (negb, bbytes) = case.get("b").unwrap();
            let (negc, cbytes) = case.get("c").unwrap();

            assert!(!nega && !negb && !negc);
            let a = $tname::from_bytes(abytes);
            let b = $tname::from_bytes(bbytes);
            let c = $tname::from_bytes(cbytes);

            match a.modinv(&b) {
                None      => assert!(false),
                Some(myc) => {
                    assert_eq!(c, myc);
                }
            }
        });
    };
}

#[cfg(test)]
macro_rules! generate_smodinv_tests {
    ($sname: ident, $tname: ident, $mname: ident) => {
        #[test]
        fn $mname() {
            generate_smodinv_tests!(body $sname, $tname, $mname);
        }
    };
    (ignore $sname: ident, $tname: ident, $mname: ident) => {
        #[test]
        #[ignore]
        fn $mname() {
            generate_smodinv_tests!(body $sname, $tname, $mname);
        }
    };
    (body $sname: ident, $tname: ident, $mname: ident) => {
        let fname = build_test_path("modinv", stringify!($sname));
        run_test(fname.to_string(), 3, |case| {
            let (nega, abytes) = case.get("a").unwrap();
            let (negb, bbytes) = case.get("b").unwrap();
            let (negc, cbytes) = case.get("c").unwrap();

            assert!(!negb && !negc);
            let a = $sname::new(*nega, $tname::from_bytes(abytes));
            let b = $sname::new(false, $tname::from_bytes(bbytes));
            let c = $sname::new(false, $tname::from_bytes(cbytes));

            match a.modinv(&b) {
                None      => assert!(false),
                Some(myc) => {
                    assert_eq!(c, myc);
                }
            }
        });
    };
}