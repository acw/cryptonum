use std::cmp::*;

pub fn compare(a: &[u64], b: &[u64]) -> Ordering {
    assert_eq!(a.len(), b.len(), "Incompatible numbers in comparison");

    let mut i = (a.len() - 1) as isize;

    while i >= 0 {
        let iu = i as usize;
        match a[iu].cmp(&b[iu]) {
            Ordering::Greater => return Ordering::Greater,
            Ordering::Less    => return Ordering::Less,
            Ordering::Equal   => i -= 1
        }
    }

    Ordering::Equal
}

macro_rules! cmp_impls {
    ($name: ident) => {
        impl PartialEq for $name {
            fn eq(&self, rhs: &$name) -> bool {
                compare(&self.value, &rhs.value) == Ordering::Equal
            }
        }

        impl Eq for $name {}

        impl PartialOrd for $name {
            fn partial_cmp(&self, rhs: &$name) -> Option<Ordering> {
                Some(compare(&self.value, &rhs.value))
            }
        }

        impl Ord for $name {
            fn cmp(&self, rhs: &$name) -> Ordering {
                compare(&self.value, &rhs.value)
            }
        }
    }
}

#[cfg(test)]
macro_rules! generate_cmp_tests {
    ($name: ident, $lname: ident) => {
        #[test]
        fn $lname() {
            let fname = format!("testdata/cmp/{}.tests", stringify!($name));
            run_test(fname.to_string(), 5, |case| {
                let (neg0, abytes) = case.get("a").unwrap();
                let (neg1, bbytes) = case.get("b").unwrap();
                let (neg2, ebytes) = case.get("e").unwrap();
                let (neg3, gbytes) = case.get("g").unwrap();
                let (neg4, lbytes) = case.get("l").unwrap();
                assert!(!neg0 && !neg1 && !neg2 && !neg3 && !neg4);
                let a = $name::from_bytes(abytes);
                let b = $name::from_bytes(bbytes);
                let e = 1 == ebytes[0];
                let g = 1 == gbytes[0];
                let l = 1 == lbytes[0];

                assert_eq!(e, a == b);
                assert_eq!(g, a >  b);
                assert_eq!(l, a <  b);

                assert_eq!(e || g, a >= b);
                assert_eq!(e || l, a <= b);
            });
        }
    };
}