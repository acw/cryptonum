pub trait ModMul<T> {
    fn modmul(&self, x: &Self, m: &T) -> Self;
}

macro_rules! modmul_impls {
    ($name: ident, $dbl: ident) => {
        impl ModMul<$name> for $name {
            fn modmul(&self, x: &$name, m: &$name) -> $name {
                let mulres      = (self as &$name) * x;
                let bigm        = $dbl::from(m);
                let (_, bigres) = mulres.divmod(&bigm);
                $name::from(bigres)
            }
        }
    };
}

#[cfg(test)]
macro_rules! generate_modmul_tests {
    ($name: ident, $lname: ident) => {
        #[test]
        fn $lname() {
            let fname = format!("testdata/modmul/{}.tests", stringify!($name));
            run_test(fname.to_string(), 4, |case| {
                let (neg0, abytes) = case.get("a").unwrap();
                let (neg1, bbytes) = case.get("b").unwrap();
                let (neg2, mbytes) = case.get("m").unwrap();
                let (neg3, cbytes) = case.get("c").unwrap();
                assert!(!neg0 && !neg1 && !neg2 && !neg3);

                let a = $name::from_bytes(abytes);
                let b = $name::from_bytes(bbytes);
                let m = $name::from_bytes(mbytes);
                let c = $name::from_bytes(cbytes);
                assert_eq!(c, a.modmul(&b, &m));
            });
        }
    };
}