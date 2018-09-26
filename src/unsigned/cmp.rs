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