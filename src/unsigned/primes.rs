use rand::RngCore;

/// Functions related to the generation of random numbers and primes.
pub trait PrimeGen: Sized + PartialOrd {
    /// Generate a random prime number, using the given RNG and running
    /// the primality check for the given number of iterations. This is
    /// equivalent to calling `random_primef` with the identity function
    /// as the modifier.
    fn random_prime<R: RngCore>(rng: &mut R, iters: usize) -> Self {
        Self::random_primef(rng, iters, |x| Some(x))
    }
    /// Generate a random prime number, using a modification function
    /// and running the primality check for the given number of iterations.
    /// The modifier function is run after the routine generates a random
    /// number, but before the primality check, and can be used to force
    /// the return value to have certain properties: the low bit set, the
    /// high bit set, and/or the number is above a certain value.
    fn random_primef<F,R>(rng: &mut R, iters: usize, prep: F) -> Self
      where F: Fn(Self) -> Option<Self>, R: RngCore;
    /// Determine if the given number is probably prime. This should be
    /// an implementation of Miller-Rabin, with some quick sanity checks,
    /// over the given number of iterations.
    fn probably_prime<R: RngCore>(&self, rng: &mut R, iters: usize) -> bool;
}

pub static SMALL_PRIMES: [u64; 310] = [
      2,     3,     5,     7,    11,    13,    17,    19,    23,    29,
     31,    37,    41,    43,    47,    53,    59,    61,    67,    71,
     73,    79,    83,    89,    97,   101,   103,   107,   109,   113,
    127,   131,   137,   139,   149,   151,   157,   163,   167,   173,
    179,   181,   191,   193,   197,   199,   211,   223,   227,   229,
    233,   239,   241,   251,   257,   263,   269,   271,   277,   281,
    283,   293,   307,   311,   313,   317,   331,   337,   347,   349,
    353,   359,   367,   373,   379,   383,   389,   397,   401,   409,
    419,   421,   431,   433,   439,   443,   449,   457,   461,   463,
    467,   479,   487,   491,   499,   503,   509,   521,   523,   541,
    547,   557,   563,   569,   571,   577,   587,   593,   599,   601,
    607,   613,   617,   619,   631,   641,   643,   647,   653,   659,
    661,   673,   677,   683,   691,   701,   709,   719,   727,   733,
    739,   743,   751,   757,   761,   769,   773,   787,   797,   809,
    811,   821,   823,   827,   829,   839,   853,   857,   859,   863,
    877,   881,   883,   887,   907,   911,   919,   929,   937,   941,
    947,   953,   967,   971,   977,   983,   991,   997,  1009,  1013,
   1019,  1021,  1031,  1033,  1039,  1049,  1051,  1061,  1063,  1069,
   1087,  1091,  1093,  1097,  1103,  1109,  1117,  1123,  1129,  1151,
   1153,  1163,  1171,  1181,  1187,  1193,  1201,  1213,  1217,  1223,
   1229,  1231,  1237,  1249,  1259,  1277,  1279,  1283,  1289,  1291,
   1297,  1301,  1303,  1307,  1319,  1321,  1327,  1361,  1367,  1373,
   1381,  1399,  1409,  1423,  1427,  1429,  1433,  1439,  1447,  1451,
   1453,  1459,  1471,  1481,  1483,  1487,  1489,  1493,  1499,  1511,
   1523,  1531,  1543,  1549,  1553,  1559,  1567,  1571,  1579,  1583,
   1597,  1601,  1607,  1609,  1613,  1619,  1621,  1627,  1637,  1657,
   1663,  1667,  1669,  1693,  1697,  1699,  1709,  1721,  1723,  1733,
   1741,  1747,  1753,  1759,  1777,  1783,  1787,  1789,  1801,  1811,
   1823,  1831,  1847,  1861,  1867,  1871,  1873,  1877,  1879,  1889,
   1901,  1907,  1913,  1931,  1933,  1949,  1951,  1973,  1979,  1987,
   1993,  1997,  1999,  2003,  2011,  2017,  2027,  2029,  2039,  2053];

macro_rules! prime_gen_impls {
    ($name: ident) => {
        impl PrimeGen for $name {
            fn random_primef<F,R>(rng: &mut R, iters: usize, modifier: F) -> Self
             where
              F: Fn($name) -> Option<$name>,
              R: RngCore
            {
                loop {
                    let base = rng.gen();

                    if let Some(candidate) = modifier(base) {
                        let good = candidate.probably_prime(rng, iters);

                        if good {
                            return candidate;
                        }
                    }
                }
            }

            fn probably_prime<R: RngCore>(&self, rng: &mut R, iters: usize) -> bool
            {
                for tester in SMALL_PRIMES.iter() {
                    if self.is_multiple_of(*tester) {
                        return false;
                    }
                }
                self.miller_rabin(rng, iters)
            }
        }

        impl $name {
            fn miller_rabin<R: RngCore>(&self, rng: &mut R, iters: usize) -> bool
            {
                let one = $name::from(1u64);
                let two = $name::from(2u64);
                let nm1 = self - $name::from(1u64);
                // Quoth Wikipedia:
                // write n - 1 as 2^r*d with d odd by factoring powers of 2 from n - 1
                let mut d = nm1.clone();
                let mut r = 0;
                while d.is_even() {
                    d >>= 1;
                    r += 1;
                    assert!(r < $name::bit_length());
                }
                // WitnessLoop: repeat k times
                'WitnessLoop: for _k in 0..iters {
                    // pick a random integer a in the range [2, n - 2]
                    let a = rng.gen_range(&two, &nm1);
                    // x <- a^d mod n
                    let mut x = a.modexp(&d, self);
                    // if x = 1 or x = n - 1 then
                    if (&x == &one) || (&x == &nm1) {
                        // continue WitnessLoop
                        continue 'WitnessLoop;
                    }
                    // repeat r - 1 times:
                    for _i in 0..r {
                        // x <- x^2 mod n
                        x = x.modexp(&two, self);
                        // if x = 1 then
                        if &x == &one {
                            // return composite
                            return false;
                        }
                        // if x = n - 1 then
                        if &x == &nm1 {
                            // continue WitnessLoop
                            continue 'WitnessLoop;
                        }
                    }
                    // return composite
                    return false;
                }
                // return probably prime
                true
            }

            fn is_multiple_of(&self, x: u64) -> bool
            {
                (self % $name::from(x)).is_zero()
            }
        }
    };
}