/// Modular squaring
pub trait ModSquare<T> {
    /// Modular squaring using the given modulus type. If it's possible,
    /// we suggest using Barrett values, which are much faster than doing
    /// modulo with the number types.
    fn modsq(&self, m: &T) -> Self;
}

macro_rules! modsq_impls {
    ($name: ident, $dbl: ident, $barrett: ident) => {
        impl ModSquare<$name> for $name {
            fn modsq(&self, m: &$name) -> $name {
                let bigsquare = self.square();
                let bigm      = $dbl::from(m);
                let (_, res)  = bigsquare.divmod(&bigm);
                $name::from(res)
            }
        }

        impl ModSquare<$barrett> for $name {
            fn modsq(&self, m: &$barrett) -> $name {
                let bigsquare = self.square();
                m.reduce(&bigsquare)
            }
        }
    };
}

#[cfg(test)]
macro_rules! generate_modsq_tests {
    ($name: ident, $lname: ident) => {
        #[test]
        fn $lname() {
            generate_modsq_tests!(body $name, $lname);
        }
    };
    (ignore $name: ident, $lname: ident) => {
        #[test]
        #[ignore]
        fn $lname() {
            generate_modsq_tests!(body $name, $lname);
        }
    };
    (body $name: ident, $lname: ident) => {
        let fname = format!("testdata/modsq/{}.tests", stringify!($name));
        run_test(fname.to_string(), 5, |case| {
            let (neg0, abytes) = case.get("a").unwrap();
            let (neg1, mbytes) = case.get("m").unwrap();
            let (neg2, cbytes) = case.get("c").unwrap();
            assert!(!neg0 && !neg1 && !neg2);

            let a = $name::from_bytes(abytes);
            let m = $name::from_bytes(mbytes);
            let c = $name::from_bytes(cbytes);
            assert_eq!(c, a.modsq(&m));
        });
    };
}

#[cfg(test)]
macro_rules! generate_barrett_modsq_tests {
    ($name: ident, $lname: ident, $bname: ident) => {
        #[test]
        fn $lname() {
            generate_barrett_modsq_tests!(body $name, $lname, $bname);
        }
    };
    (ignore $name: ident, $lname: ident, $bname: ident) => {
        #[test]
        #[ignore]
        fn $lname() {
            generate_barrett_modsq_tests!(body $name, $lname, $bname);
        }
    };
    (body $name: ident, $lname: ident, $bname: ident) => {
        let fname = format!("testdata/modsq/{}.tests", stringify!($name));
        run_test(fname.to_string(), 5, |case| {
            let (neg0, abytes) = case.get("a").unwrap();
            let (neg1, mbytes) = case.get("m").unwrap();
            let (neg2, cbytes) = case.get("c").unwrap();
            let (neg3, kbytes) = case.get("k").unwrap();
            let (neg4, ubytes) = case.get("u").unwrap();
            assert!(!neg0 && !neg1 && !neg2 && !neg3 && !neg4);

            let a    = $name::from_bytes(abytes);
            let m    = $name::from_bytes(mbytes);
            let c    = $name::from_bytes(cbytes);
            let kbig = $name::from_bytes(kbytes);
            let k    = usize::from(kbig);
            let mu   = $bname::from_bytes(ubytes);
            let bar  = $name::new_barrett(k, $bname::from(m), mu);

            if k == a.value.len() {
                assert_eq!(c, a.modsq(&bar));
            }
        });
    };
}