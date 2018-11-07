macro_rules! barrett_impl {
    ($bar: ident, $name: ident, $name64: ident, $dbl: ident, $dbl64: ident) => {
        #[derive(PartialEq)]
        pub struct $bar {
            pub(crate) k:  usize,
            pub(crate) m:  $name64,
            pub(crate) mu: $name64
        }

        impl $bar {
            /// Generate a new Barrett number from the given input. This operation
            /// is relatively slow, so you should only do it if you plan to use 
            /// reduce() multiple times with the ame number.
            pub fn new(m: $name) -> $bar {
                // Step #1: Figure out k
                let mut k = 0;
                for i in 0..m.value.len() {
                    if m.value[i] != 0 {
                        k = i;
                    }
                }
                k += 1;
                // Step #2: Compute b
                let mut b = $dbl64::zero();
                b.value[2*k] = 1;
                // Step #3: Divide b by m.
                let bigm = $dbl64::from(&m);
                let quot = b / &bigm;
                let resm = $name64::from(&m);
                let mu = $name64::from(&quot);
                // Done!
                $bar { k: k, m: resm, mu: mu }
            }

            /// Generate a new Barrett number from its component parts. You
            /// probably don't want to use it; it's mostly here for debugging
            /// purposes, so that tests won't have to compute this all the
            /// time.
            pub fn from_components(k: usize, m: $name64, mu: $name64) -> $bar {
                $bar { k: k, m: m, mu: mu }
            }

            // Reduce the input by this value; in other words, perform a mod
            // operation.
            #[inline(always)]
            pub fn reduce(&self, x: &$dbl) -> $name {
                // 1. q1←⌊x/bk−1⌋, q2←q1 · μ, q3←⌊q2/bk+1⌋.
                let     q1: $name64 = $name64::from(x >> ((self.k - 1) * 64));
                let     q2          = q1 * &self.mu;
                let     q3: $name64 = $name64::from(q2 >> ((self.k + 1) * 64));
                // 2. r1←x mod bk+1, r2←q3 · m mod bk+1, r←r1 − r2.
                let mut r:  $dbl64  = $dbl64::from(x);
                r.mask(self.k + 1);
                let mut r2: $dbl64  = $dbl64::from(q3 * &self.m);
                r2.mask(self.k + 1);
                let went_negative = &r < &r2;
                r -= &r2;
                // 3. If r<0 then r←r+bk+1.
                if went_negative {
                    let mut bk1 = $dbl64::zero();
                    bk1.value[self.k+1] = 1;
                    // this may overflow, and we should probably be OK with it.
                    r += &bk1;
                }
                // 4. While r≥m do: r←r−m.
                let m2 = $dbl64::from(&self.m);
                while &r > &m2 {
                    r -= &m2;
                }
                // Done!
                $name::from(&r)
            }
        }

        impl $name {
            /// Generate a Barrett number from this number.
            pub fn generate_barrett(&self) -> $bar {
                $bar::new(self.clone())
            }

            #[cfg(test)]
            pub(crate) fn new_barrett(k: usize, m: $name64, mu: $name64) -> $bar {
                $bar{ k: k, m: m, mu: mu }
            }
        }

        impl fmt::Debug for $bar {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.debug_struct(stringify!($name))
                 .field("k", &self.k)
                 .field("m", &self.m)
                 .field("mu", &self.mu)
                 .finish()
            }
        }
    };
}

#[cfg(test)]
macro_rules! generate_barrett_gen_tests {
    ($name: ident, $lname: ident, $bname: ident) => {
        #[test]
        fn $lname() {
            generate_barrett_gen_tests!(body $name, $lname, $bname);
        }
    };
    (ignore $name: ident, $lname: ident, $bname: ident) => {
        #[test]
        #[ignore]
        fn $lname() {
            generate_barrett_gen_tests!(body $name, $lname, $bname);
        }
    };
    (body $name: ident, $lname: ident, $bname: ident) => {
        let fname = format!("testdata/barrett_gen/{}.tests", stringify!($name));
        run_test(fname.to_string(), 3, |case| {
            let (neg0, mbytes) = case.get("m").unwrap();
            let (neg1, kbytes) = case.get("k").unwrap();
            let (neg2, ubytes) = case.get("u").unwrap();
            assert!(!neg0 && !neg1 && !neg2);

            let m    = $name::from_bytes(mbytes);
            let kbig = $name::from_bytes(kbytes);
            let mu   = $bname::from_bytes(ubytes);
            //
            let mbig = $bname::from(&m);
            let k    = usize::from(&kbig);
            //
            let bar  = m.generate_barrett();
            assert_eq!(k,    bar.k);
            assert_eq!(mbig, bar.m);
            assert_eq!(mu,   bar.mu);
        });
    };
}

#[cfg(test)]
macro_rules! generate_barrett_red_tests {
    ($name: ident, $lname: ident, $bname: ident, $dbl: ident) => {
        #[test]
        fn $lname() {
            generate_barrett_red_tests!(body $name, $lname, $bname, $dbl);
        }
    };
    (ignore $name: ident, $lname: ident, $bname: ident, $dbl: ident) => {
        #[test]
        #[ignore]
        fn $lname() {
            generate_barrett_red_tests!(body $name, $lname, $bname, $dbl);
        }
    };
    (body $name: ident, $lname: ident, $bname: ident, $dbl: ident) => {
        let fname = format!("testdata/barrett_reduce/{}.tests", stringify!($name));
        run_test(fname.to_string(), 5, |case| {
            let (neg0, mbytes) = case.get("m").unwrap();
            let (neg1, kbytes) = case.get("k").unwrap();
            let (neg2, ubytes) = case.get("u").unwrap();
            let (neg3, xbytes) = case.get("x").unwrap();
            let (neg4, rbytes) = case.get("r").unwrap();
            assert!(!neg0 && !neg1 && !neg2 && !neg3 && !neg4);

            let m    = $name::from_bytes(mbytes);
            let kbig = $name::from_bytes(kbytes);
            let k    = usize::from(&kbig);
            let mu   = $bname::from_bytes(ubytes);
            let bar  = $name::new_barrett(usize::from(k), $bname::from(m), mu);
            let x    = $dbl::from_bytes(xbytes);
            let r    = $name::from_bytes(rbytes);
            //
            assert_eq!(r, bar.reduce(&x));
        });
    };
}