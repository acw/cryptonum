macro_rules! barrett_impl {
    ($bar: ident, $name: ident, $name64: ident, $dbl: ident, $dbl64: ident) => {
       pub struct $bar {
           pub(crate) k:  usize,
           pub(crate) m:  $name64,
           pub(crate) mu: $name64
       }

       impl $bar {
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
           
           pub fn reduce(&self, x: &$dbl) -> $name {
                let     m2: $dbl64  = $dbl64::from(&self.m);
                // 1. q1←⌊x/bk−1⌋, q2←q1 · μ, q3←⌊q2/bk+1⌋.
                let     q1: $name64 = $name64::from(x >> (self.k - 1));
                let     q2: $dbl64  = $dbl64::from(q1 * &self.mu);
                let     q3: $name64 = $name64::from(q2 >> (self.k + 1));
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
                    r += &bk1;
                }
                // 4. While r≥m do: r←r−m.
                while &r > &m2 {
                    r -= &m2;
                }
                // Done!
                $name::from(&r)
           }
       }
    };
}
