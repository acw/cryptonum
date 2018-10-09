pub trait Square<Output> {
    fn square(&self) -> Output;
}

macro_rules! square_impls {
    ($name: ident, $bigger: ident, $size: expr) => {
        impl Square<$bigger> for $name {
            fn square(&self) -> $bigger {
               let mut w = [0; $size/32];
               let     t = $size / 64;

               for i in 0..t {
                   let x128 = self.value[i] as u128;
                   let mut uvb = (w[2*i] as u128) + (x128 * x128);
                   w[2*i] = uvb & 0xFFFFFFFFFFFFFFFF;
                   let mut c = uvb >> 64;
                   for j in (i+1)..t {
                       let xj128  = self.value[j] as u128;
                       let xi128  = self.value[i] as u128;
                       // this first product is safely 128 bits or less,
                       // because the input arguments are both 64 bits.
                       let xij128 = xj128 * xi128;
                       // this next bit may overflow, but will do so by exactly
                       // one bit.
                       let twoxij128 = xij128 << 1;
                       let carried_shl = (xij128 & (1 << 127)) != 0;
                       // this next bit may *also* overflow, but should also do
                       // so by no more than one bit.
                       let (new,carry1) = twoxij128.overflowing_add(c);
                       // ditto ...
                       let wij = w[i+j];
                       let (uvb2,carry2) = new.overflowing_add(wij as u128);
                       // for the value we're going to save for this digit, we
                       // only care about the low bits, so we can forget about
                       // the carry stuff.
                       w[i+j] = uvb2 & 0xFFFFFFFFFFFFFFFF;
                       // for c, though, we do care about the carries, above.
                       // Fortunately, they were both by only one bit, so we
                       // should be able to just back-fix them.
                       c = uvb2 >> 64;
                       if carried_shl  { c += 1 << 64; }
                       if carry1       { c += 1 << 64; }
                       if carry2       { c += 1 << 64; }
                   }
                   w[i+t] = c;
               }
               let mut res = $bigger::zero();
               for i in 0..w.len() { res.value[i] = w[i] as u64; }
               res
            }
        }
    };
}

#[cfg(test)]
macro_rules! generate_square_tests {
    ($name: ident, $lname: ident, $dbl: ident) => {
        #[test]
        fn $lname() {
            generate_square_tests!(body $name, $lname, $dbl);
        }
    };
    (ignore $name: ident, $lname: ident, $dbl: ident) => {
        #[test]
        #[ignore]
        fn $lname() {
            generate_square_tests!(body $name, $lname, $dbl);
        }
    };
    (body $name: ident, $lname: ident, $dbl: ident) => {
        let fname = format!("testdata/square/{}.tests", stringify!($name));
        run_test(fname.to_string(), 2, |case| {
            let (neg0, abytes) = case.get("a").unwrap();
            let (neg1, rbytes) = case.get("r").unwrap();
            assert!(!neg0 && !neg1);

            let a = $name::from_bytes(abytes);
            let r = $dbl::from_bytes(rbytes);
            assert_eq!(r, a.square());
        });
    };
}