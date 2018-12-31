/// Computations of the modular inverse.
pub trait ModDiv: Sized {
    /// Compute the modular division of the number by the given number and
    /// modulus. The divisor *must* have a modular inverse for this function
    /// to succeed.
    fn moddiv(&self, divisor: &Self, phi: &Self) -> Self;
}

macro_rules! moddiv_impls {
    ($sname: ident, $dbl: ident) => {
        impl ModDiv for $sname {
            fn moddiv(&self, divisor: &Self, phi: &Self) -> Self
            {
                let safe_divisor = divisor % phi;
                let unsigned_i = safe_divisor.value.modinv(&phi.value).expect("no modular inverse of moddiv divisor");
                let i = $sname::new(divisor.negative, unsigned_i);
                let selfi = i * self;
                $sname::from( selfi % $dbl::from(phi) )
            }
        }
    };
}

#[cfg(test)]
macro_rules! generate_moddiv_tests {
    ($sname: ident, $tname: ident, $mname: ident) => {
        #[test]
        fn $mname() {
            generate_moddiv_tests!(body $sname, $tname, $mname);
        }
    };
    (ignore $sname: ident, $tname: ident, $mname: ident) => {
        #[test]
        #[ignore]
        fn $mname() {
            generate_moddiv_tests!(body $sname, $tname, $mname);
        }
    };
    (body $sname: ident, $tname: ident, $mname: ident) => {
        let fname = build_test_path("moddiv", stringify!($sname));
        run_test(fname.to_string(), 4, |case| {
            let (nega, abytes) = case.get("a").unwrap();
            let (negb, bbytes) = case.get("b").unwrap();
            let (negm, mbytes) = case.get("m").unwrap();
            let (negc, cbytes) = case.get("c").unwrap();

            let a = $sname::new(*nega, $tname::from_bytes(abytes));
            let b = $sname::new(*negb, $tname::from_bytes(bbytes));
            let m = $sname::new(*negm, $tname::from_bytes(mbytes));
            let c = $sname::new(*negc, $tname::from_bytes(cbytes));
            let res = a.moddiv(&b, &m);
            assert_eq!(c, res);
        });
    };
}