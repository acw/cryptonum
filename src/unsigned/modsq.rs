pub trait ModSquare<T> {
    fn modsq(&self, m: &T) -> Self;
}

macro_rules! modsq_impls {
    ($name: ident, $dbl: ident) => {
        impl ModSquare<$name> for $name {
            fn modsq(&self, m: &$name) -> $name {
                let bigsquare = self.square();
                let bigm      = $dbl::from(m);
                let (_, res)  = bigsquare.divmod(&bigm);
                $name::from(res)
            }
        }
    };
}

#[cfg(test)]
macro_rules! generate_modsq_tests {
    ($name: ident, $lname: ident) => {
        #[test]
        fn $lname() {
            let fname = format!("testdata/modsq/{}.tests", stringify!($name));
            run_test(fname.to_string(), 3, |case| {
                let (neg0, abytes) = case.get("a").unwrap();
                let (neg1, mbytes) = case.get("m").unwrap();
                let (neg2, cbytes) = case.get("c").unwrap();
                assert!(!neg0 && !neg1 && !neg2);

                let a = $name::from_bytes(abytes);
                let m = $name::from_bytes(mbytes);
                let c = $name::from_bytes(cbytes);
                assert_eq!(c, a.modsq(&m));
            });
        }
    };
}