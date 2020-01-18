{-# LANGUAGE QuasiQuotes #-}
module ModInv(
    generateModInvOps
  )
 where

import File
import Language.Rust.Data.Ident
import Language.Rust.Data.Position
import Language.Rust.Quote
import Language.Rust.Syntax

generateModInvOps :: File
generateModInvOps = File {
    predicate = \ me others -> (me + 64) `elem` others,
    outputName = "modinv",
    isUnsigned = False,
    generator = declareModInv,
    testCase = Nothing
}

declareModInv :: Word -> [Word] -> SourceFile Span 
declareModInv bitsize _ =
    let sname = mkIdent ("I" ++ show bitsize)
        uname = mkIdent ("U" ++ show bitsize)
    in [sourceFile|
        use core::convert::TryFrom;
        use crate::CryptoNum;
        use crate::signed::$$sname;
        use crate::unsigned::$$uname;

        impl $$uname {
            fn modinv(&self, phi: &$$uname) -> Option<$$uname>
            {
                let (_, mut b, g) = phi.egcd(&self);

                if g != $$sname::from(1i64) {
                    return None;
                }

                let sphi = $$sname::try_from(phi).expect("over/underflow in modinv phi");

                while b.is_negative() {
                    b += &sphi;
                }

                if b > sphi {
                    b -= &sphi;
                }

                Some($$uname::try_from(b).expect("overflow/underflow in modinv result"))
            }

            fn egcd(&self, rhs: &$$uname) -> ($$sname, $$sname, $$sname) {
                // INPUT: two positive integers x and y.
                let mut x = $$sname::try_from(self).expect("overflow in modinv base");
                let mut y = $$sname::try_from(rhs).expect("overflow in modinv rhs");
                // OUTPUT: integers a, b, and v such that ax + by = v,
                //         where v = gcd(x, y).
                // 1. g←1.
                let mut gshift: usize = 0;
                // 2. While x and y are both even, do the following: x←x/2,
                //    y←y/2, g←2g.
                while x.is_even() && y.is_even() {
                    x >>= 1u64;
                    y >>= 1u64;
                    gshift += 1;
                }
                // 3. u←x, v←y, A←1, B←0, C←0, D←1.
                let mut u = x.clone();
                let mut v = y.clone();
                #[allow(non_snake_case)]
                let mut A = $$sname::from(1i64);
                #[allow(non_snake_case)]
                let mut B = $$sname::zero();
                #[allow(non_snake_case)]
                let mut C = $$sname::zero();
                #[allow(non_snake_case)]
                let mut D = $$sname::from(1i64);
                loop {
                    // 4. While u is even do the following:
                    while u.is_even() {
                        // 4.1 u←u/2.
                        u >>= 1u64;
                        // 4.2 If A≡B≡0 (mod 2) then A←A/2, B←B/2; otherwise,
                        //     A←(A + y)/2, B←(B − x)/2.
                        if A.is_even() && B.is_even() {
                            A >>= 1u64;
                            B >>= 1u64;
                        } else {
                            A += &y;
                            A >>= 1u64;
                            B -= &x;
                            B >>= 1u64;
                        }
                    }
                    // 5. While v is even do the following:
                    while v.is_even() {
                        // 5.1 v←v/2.
                        v >>= 1u64;
                        // 5.2 If C ≡ D ≡ 0 (mod 2) then C←C/2, D←D/2; otherwise,
                        //     C←(C + y)/2, D←(D − x)/2.
                        if C.is_even() && D.is_even() {
                            C >>= 1u64;
                            D >>= 1u64;
                        } else {
                            C += &y;
                            C >>= 1u64;
                            D -= &x;
                            D >>= 1u64;
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

            fn gcd_is_one(&self, b: &$$uname) -> bool {
                let mut u = self.clone();
                let mut v = b.clone();
                let one = $$uname::from(1u64);

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
                    u >>= 1u64;
                }

                loop {
                    while v.is_even() {
                        v >>= 1u64;
                    }
                    // u and v guaranteed to be odd right now.
                    if u > v {
                        // make sure that v > u, so that our subtraction works
                        // out.
                        let t = u;
                        u = v;
                        v = t;
                    }
                    v -= &u;

                    if v.is_zero() {
                        return u == one;
                    }
                }
            }
        }
    |]