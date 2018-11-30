/// Computations of the modular inverse.
pub trait ModInv: Sized {
    /// Compute the modular inverse of this number under the given
    /// modulus, if it exists. If self is a, the modulus / argument
    /// is phi, and the result is Some(m), then (a * m) % phi = 1.
    fn modinv(&self, phi: &Self) -> Option<Self>;
}

macro_rules! modinv_impls {
    ($name: ident, $sname: ident, $uname: ident) => {
        impl ModInv for $name {
            fn modinv(&self, phi: &$name) -> Option<$name>
            {
                println!("---");
                let (_, mut b, g) = phi.egcd(&self);

                if g != $sname::from(1i64) {
                    return None;
                }

                while b.is_negative() {
                    println!("UPTICK");
                    b += $sname::from($uname::from(phi));
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

            println!("a: {:X}", a);
            println!("b: {:X}", b);
            println!("c: {:X}", c);
            match a.modinv(&b) {
                None      => assert!(false),
                Some(myc) => {
                    println!("d: {:X}", myc);
                    assert_eq!(c, myc);
                }
            }
        });
    };
}