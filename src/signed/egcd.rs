/// GCD computations, with extended information
pub trait EGCD<T> {
    /// Compute the extended GCD for this value and the given value.
    /// If the inputs to this function are x (self) and y (the argument),
    /// and the results are (a, b, g), then (a * x) + (b * y) = g.
    fn egcd(&self, rhs: &Self) -> (T, T, T);
    /// Compute whether or not the given number and the provided number
    /// have a GCD of 1. This is a slightly faster version of calling
    /// `egcd` and testing the result, because it can ignore some
    /// intermediate values.
    fn gcd_is_one(&self, &Self) -> bool;
}

macro_rules! egcd_impls {
    ($sname: ident, $name: ident, $ssmall: ident) => {
        impl EGCD<$sname> for $name {
            fn egcd(&self, rhs: &$name) -> ($sname, $sname, $sname) {
                // INPUT: two positive integers x and y.
                let mut x = $sname::from($ssmall::from(self));
                let mut y = $sname::from($ssmall::from(rhs));
                // OUTPUT: integers a, b, and v such that ax + by = v,
                //         where v = gcd(x, y).
                // 1. g←1.
                let mut gshift = 0;
                // 2. While x and y are both even, do the following: x←x/2,
                //    y←y/2, g←2g.
                while x.is_even() && y.is_even() {
                    x >>= 1;
                    y >>= 1;
                    gshift += 1;
                }
                // 3. u←x, v←y, A←1, B←0, C←0, D←1.
                let mut u = x.clone();
                let mut v = y.clone();
                #[allow(non_snake_case)]
                let mut A = $sname::from(1i64);
                #[allow(non_snake_case)]
                let mut B = $sname::zero();
                #[allow(non_snake_case)]
                let mut C = $sname::zero();
                #[allow(non_snake_case)]
                let mut D = $sname::from(1i64);
                loop {
                    // 4. While u is even do the following:
                    while u.is_even() {
                        // 4.1 u←u/2.
                        u >>= 1;
                        // 4.2 If A≡B≡0 (mod 2) then A←A/2, B←B/2; otherwise,
                        //     A←(A + y)/2, B←(B − x)/2.
                        if A.is_even() && B.is_even() {
                            A >>= 1;
                            B >>= 1;
                        } else {
                            A += &y;
                            A >>= 1;
                            B -= &x;
                            B >>= 1;
                        }
                    }
                    // 5. While v is even do the following:
                    while v.is_even() {
                        // 5.1 v←v/2.
                        v >>= 1;
                        // 5.2 If C ≡ D ≡ 0 (mod 2) then C←C/2, D←D/2; otherwise,
                        //     C←(C + y)/2, D←(D − x)/2.
                        if C.is_even() && D.is_even() {
                            C >>= 1;
                            D >>= 1;
                        } else {
                            C += &y;
                            C >>= 1;
                            D -= &x;
                            D >>= 1;
                        }
                    }
                    // 6. If u≥v then u←u−v, A←A−C,B←B−D;
                    //       otherwise,v←v−u, C←C−A, D←D−B.
                    if u >= v {
                        u -= &v;
                        A -= &C;
                        B -= &D;
                    } else {
                        v -= &u;
                        C -= &A;
                        D -= &B;
                    }
                    // 7. If u = 0, then a←C, b←D, and return(a, b, g · v);
                    //        otherwise, go to step 4.
                    if u.is_zero() {
                        return (C, D, v << gshift);
                    }
                }
            }

            fn gcd_is_one(&self, b: &$name) -> bool {
                let mut u = self.clone();
                let mut v = b.clone();
                let one = $name::from(1u64);

                if u.is_zero() {
                    return v == one;
                }

                if v.is_zero() {
                    return u == one;
                }

                if u.is_even() && v.is_even() {
                    return false;
                }

                while u.is_even() {
                    u >>= 1;
                }

                loop {
                    while v.is_even() {
                        v >>= 1;
                    }
                    // u and v guaranteed to be odd right now.
                    if u > v {
                        // make sure that v > u, so that our subtraction works
                        // out.
                        let t = u;
                        u = v;
                        v = t;
                    }
                    v = v - &u;

                    if v.is_zero() {
                        return u == one;
                    }
                }
            }
        }
    };
}

#[cfg(test)]
macro_rules! generate_egcd_tests {
    ($sname: ident, $uname: ident, $tname: ident, $sname64: ident, $uname64: ident) => {
        #[test]
        fn $tname() {
            generate_egcd_tests!(body $sname, $uname, $tname, $sname64, $uname64);
        }
    };
    (ignore $sname: ident, $uname: ident, $tname: ident, $sname64: ident, $uname64: ident) => {
        #[test]
        #[ignore]
        fn $tname() {
            generate_egcd_tests!(body $sname, $uname, $tname, $sname64, $uname64);
        }
    };
    (body $sname: ident, $uname: ident, $tname: ident, $sname64: ident, $uname64: ident) => {
        let fname = build_test_path("egcd", stringify!($sname));
        run_test(fname.to_string(), 5, |case| {
            let (negx, xbytes) = case.get("x").unwrap();
            let (negy, ybytes) = case.get("y").unwrap();
            let (negv, vbytes) = case.get("v").unwrap();
            let (nega, abytes) = case.get("a").unwrap();
            let (negb, bbytes) = case.get("b").unwrap();

            assert!(!negx && !negy);
            let x = $uname::from_bytes(xbytes);
            let y = $uname::from_bytes(ybytes);
            let v = $sname64::new(*negv, $uname64::from_bytes(vbytes));
            let a = $sname64::new(*nega, $uname64::from_bytes(abytes));
            let b = $sname64::new(*negb, $uname64::from_bytes(bbytes));
            let (mya, myb, myv) = x.egcd(&y);
            assert_eq!(v, myv, "GCD test");
            assert_eq!(a, mya, "X factor test");
            assert_eq!(b, myb, "Y factor tst");
            assert_eq!(x.gcd_is_one(&y), (myv == $sname64::from(1i64)));
        });
     };
}
