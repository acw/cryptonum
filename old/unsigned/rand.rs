macro_rules! random_impls {
    ($name: ident, $uniform: ident) => {
        impl Distribution<$name> for Standard {
            fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> $name
            {
                let mut res = $name::zero();

                for x in res.value.iter_mut() {
                    *x = rng.next_u64();
                }

                res
            }
        }

        pub struct $uniform {
            low: $name,
            high: $name,
            inclusive: bool
        }

        impl UniformSampler for $uniform {
            type X = $name;

            fn new<B1,B2>(low: B1, high: B2) -> Self
             where B1: SampleBorrow<Self::X> + Sized,
                   B2: SampleBorrow<Self::X> + Sized
            {
                $uniform {
                    low: low.borrow().clone(),
                    high: high.borrow().clone(),
                    inclusive: false
                }
            }

            fn new_inclusive<B1, B2>(low: B1, high: B2) -> Self
                where B1: SampleBorrow<Self::X> + Sized,
                      B2: SampleBorrow<Self::X> + Sized
            {
                $uniform {
                    low: low.borrow().clone(),
                    high: high.borrow().clone(),
                    inclusive: true
                }
            }

            fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Self::X {
                loop {
                    let candidate = rng.gen();

                    if candidate < self.low {
                        continue;
                    }

                    if candidate > self.high {
                        continue;
                    }

                    if !self.inclusive && (candidate == self.high) {
                        continue;
                    }

                    return candidate;
                }
            }
        }

        impl SampleUniform for $name {
            type Sampler = $uniform;
        }
    };
}