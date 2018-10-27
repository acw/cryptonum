/// Concurrent div/mod operations for a number, so that you don't
/// have to do them separately.
pub trait DivMod
 where Self: Sized
{
    /// Compute the quotient and remainder of a and b.
    fn divmod(&self, rhs: &Self) -> (Self, Self);
}

pub fn get_number_size(v: &[u64]) -> Option<usize>
{
    for (idx, val) in v.iter().enumerate().rev() {
        if *val != 0 {
            return Some(idx);
        }
    }
    None
}

macro_rules! safesubidx
{
    ($array: expr, $index: expr, $amt: expr) => ({
        let idx = $index;
        let amt = $amt;

        if idx < amt {
            0
        } else {
            $array[idx-amt]
        }
    })
}

macro_rules! div_impls
{
    ($name: ident, $dbl: ident) => {
        impl DivMod for $name {
            fn divmod(&self, rhs: &$name) -> ($name, $name) {
                // if the divisor is larger, then the answer is pretty simple
                if rhs > self {
                    return ($name::zero(), self.clone());
                }
                // compute the basic number sizes
                let mut n = match get_number_size(&self.value) {
                                None    => 0,
                                Some(v) => v
                            };
                let t = match get_number_size(&rhs.value) {
                            None    => panic!("Division by zero!"),
                            Some(v) => v
                        };
                assert!(t <= n);
                // now generate mutable versions we can mess with
                let mut x = $dbl::from(self);
                let mut y = $dbl::from(rhs);
                // If we want this to perform reasonable, it's useful if the
                // value of y[t] is shifted so that the high bit is set.
                let lambda_shift = y.value[t].leading_zeros() as usize;
                if lambda_shift != 0 {
                    let dupx = x.value.clone();
                    let dupy = y.value.clone();

                    shiftl(&mut x.value, &dupx, lambda_shift);
                    shiftl(&mut y.value, &dupy, lambda_shift);
                    n = get_number_size(&x.value).unwrap();
                }
                // now go!
                // 1. For j from 0 to (n-t) do: q[j] = 0;
                //      [NB: I take some liberties with this concept]
                let mut q = $dbl::zero();
                // 2. While (x >= y * b^(n-t)) do the following:
                let mut ybnt = $dbl::zero();
                for i in 0..self.value.len() {
                    ybnt.value[(n-t)+i] = y.value[i];
                    if (n-t)+i >= ybnt.value.len() {
                        break;
                    }
                }
                while x > ybnt {
                    q.value[n - t] += 1;
                    x -= &ybnt;
                }
                // 3. For i from n down to t+1 do the following:
                let mut i = n;
                while i >= (t + 1) {
                    // 3.1. If x[i] = y[t]
                    if x.value[i] == y.value[t] {
                        // ... then set q[i-t-1] = b - 1
                        q.value[i-t-1] = 0xFFFFFFFFFFFFFFFF;
                    } else {
                        // ... otherwise set q[i-t-1] =
                        //        floor((x[i] * b + x[i-1]) / y[t])
                        let xib         = (x.value[i] as u128) << 64;
                        let xi1         = safesubidx!(x.value,i,1) as u128;
                        let yt          = y.value[t] as u128;
                        let qit1        = (xib + xi1) / yt;
                        q.value[i-t-1] = qit1 as u64;
                    }
                    // 3.2. While q[i-t-1] * (y[t]*b + y[t-1]) >
                    //                  (x[i] * b^2 + x[i-1] * b + x[i-2])
                    loop {
                        // three is very close to 2.
                        let     qit1    = U192::from(safesubidx!(q.value,i-t,1));
                        let mut ybits   = U192::zero();
                        ybits.value[0]  = safesubidx!(y.value, t, 1);
                        ybits.value[1]  = y.value[t];
                        let       qiybs = &qit1 * &ybits;
                        let mut   xbits = U384::zero();
                        xbits.value[0]  = safesubidx!(x.value,i,2);
                        xbits.value[1]  = safesubidx!(x.value,i,1);
                        xbits.value[2]  = x.value[i];

                        if !(&qiybs > &xbits) {
                            break;
                        }

                        // ... do q[i-t-1] = q[i-t-1] - 1
                        q.value[i-t-1] -= 1;
                    }
                    // 3.3. x = x - q[i-t-1] * y * b^(i-t-1)
                    // 3.4. If x < 0
                    //      then set x = x + y * b^(i-t-1) and
                    //               q[i-t-1] = q[i-t-1] - 1
                    let mut qbit1 = $name::zero();
                    qbit1.value[i-t-1] = q.value[i-t-1];
                    let smallery = $name::from(&y);
                    let mut subpart = &smallery * &qbit1;
                    if subpart > x {
                        let mut addback = $dbl::zero();
                        for (idx, val) in y.value.iter().enumerate() {
                            let dest = idx + (i - t - 1);
                            if dest < addback.value.len() {
                                addback.value[dest] = *val;
                            }
                        }
                        q.value[i-t-1] -= 1;
                        subpart -= &addback;
                    }
                    assert!(subpart <= x);
                    x -= &subpart;
                    i -= 1;
                }
                // 4. r = x ... sort of. Remember, we potentially did a bit of shifting
                // around at the very beginning, which we now need to account for. On the
                // bright side, we only need to account for this in the remainder.
                let mut r = $name::from(&x);
                let dupr = r.value.clone();
                shiftr(&mut r.value, &dupr, lambda_shift);
                // 5. Return (q,r)
                let resq = $name::from(&q);
                (resq, r)
            }
        }
        impl Div for $name {
            type Output = $name;

            fn div(self, rhs: $name) -> $name {
                let (res, _) = self.divmod(&rhs);
                res
            }
        }

        impl<'a> Div<&'a $name> for $name {
            type Output = $name;

            fn div(self, rhs: &$name) -> $name {
                let (res, _) = self.divmod(rhs);
                res
            }
        }

        impl<'a> Div<$name> for &'a $name {
            type Output = $name;

            fn div(self, rhs: $name) -> $name {
                let (res, _) = self.divmod(&rhs);
                res
            }
        }
        impl<'a,'b> Div<&'a $name> for &'b $name {
            type Output = $name;

            fn div(self, rhs: &$name) -> $name {
                let (res, _) = self.divmod(rhs);
                res
            }
        }

        impl Rem for $name {
            type Output = $name;

            fn rem(self, rhs: $name) -> $name {
                let (_, res) = self.divmod(&rhs);
                res
            }
        }

        impl<'a> Rem<&'a $name> for $name {
            type Output = $name;

            fn rem(self, rhs: &$name) -> $name {
                let (_, res) = self.divmod(&rhs);
                res
            }
        }

        impl<'a> Rem<$name> for &'a $name {
            type Output = $name;

            fn rem(self, rhs: $name) -> $name {
                let (_, res) = self.divmod(&rhs);
                res
            }
        }

        impl<'a,'b> Rem<&'a $name> for &'b $name {
            type Output = $name;

            fn rem(self, rhs: &$name) -> $name {
                let (_, res) = self.divmod(&rhs);
                res
            }
        }
    }
}

#[cfg(test)]
macro_rules! generate_div_tests {
    ($name: ident, $lname: ident) => {
        #[test]
        fn $lname() {
            generate_div_tests!(body $name, $lname);
        }
    };
    (ignore $name: ident, $lname: ident) => {
        #[test]
        #[ignore]
        fn $lname() {
            generate_div_tests!(body $name, $lname);
        }
    };
    (body $name: ident, $lname: ident) => {
        let fname = format!("testdata/div/{}.tests", stringify!($name));
        run_test(fname.to_string(), 4, |case| {
            let (neg0, abytes) = case.get("a").unwrap();
            let (neg1, bbytes) = case.get("b").unwrap();
            let (neg2, qbytes) = case.get("q").unwrap();
            let (neg3, rbytes) = case.get("r").unwrap();
            assert!(!neg0 && !neg1 && !neg2 && !neg3);

            let a = $name::from_bytes(abytes);
            let b = $name::from_bytes(bbytes);
            let q = $name::from_bytes(qbytes);
            let r = $name::from_bytes(rbytes);
            let (myq, myr) = a.divmod(&b);
            assert_eq!(q, myq);
            assert_eq!(r, myr);
        });
    };
}