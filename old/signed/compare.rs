macro_rules! cmp_impls {
    ($name: ident) => {
        impl PartialOrd for $name {
            fn partial_cmp(&self, rhs: &$name) -> Option<Ordering> {
                Some(self.cmp(&rhs))
            }
        }

        impl Ord for $name {
            fn cmp(&self, rhs: &$name) -> Ordering {
                match (self.negative, rhs.negative) {
                    (false, false) => self.value.cmp(&rhs.value),
                    (true,  false) => Ordering::Less,
                    (false, true)  => Ordering::Greater,
                    (true,  true)  => self.value.cmp(&rhs.value).reverse()
                }
            }
        }
    }
}

#[cfg(test)]
macro_rules! generate_sigcmp_tests {
    ($sname: ident, $name: ident, $lname: ident) => {
        #[test]
        fn $lname() {
            generate_sigcmp_tests!(body $sname, $name, $lname);
        }
    };
    (ignore $sname: ident, $name: ident, $lname: ident) => {
        #[test]
        #[ignore]
        fn $lname() {
            generate_sigcmp_tests!(body $sname, $name, $lname);
        }
    };
    (body $sname: ident, $name: ident, $lname: ident) => {
        let fname = build_test_path("sigcmp", stringify!($sname));
        run_test(fname.to_string(), 5, |case| {
            let (nega, abytes) = case.get("a").unwrap();
            let (negb, bbytes) = case.get("b").unwrap();
            let (neg2, ebytes) = case.get("e").unwrap();
            let (neg3, gbytes) = case.get("g").unwrap();
            let (neg4, lbytes) = case.get("l").unwrap();

            assert!(!neg2 && !neg3 && !neg4);
            let a = $sname::new(*nega, $name::from_bytes(abytes));
            let b = $sname::new(*negb, $name::from_bytes(bbytes));
            let e = 1 == ebytes[0];
            let g = 1 == gbytes[0];
            let l = 1 == lbytes[0];

            assert_eq!(e, a == b, "equals wrong");
            assert_eq!(g, a >  b, "greater than wrong");
            assert_eq!(l, a <  b, "less than wrong");

            assert_eq!(e || g, a >= b, "greater / equals wrong");
            assert_eq!(e || l, a <= b, "less than / equals wrong");
        });
    };
}