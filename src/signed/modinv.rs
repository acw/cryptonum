pub trait ModInv: Sized {
    fn modinv(&self, phi: &Self) -> Option<Self>;
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

                while b.is_negative() {
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
        let fname = format!("testdata/modinv/{}.tests", stringify!($sname));
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
                Some(myc) => assert_eq!(c, myc)
            }
        });
    };
}