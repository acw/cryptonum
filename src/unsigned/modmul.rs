pub trait ModMul<T> {
    fn modmul(&self, x: &Self, m: &T) -> Self;
}

macro_rules! modmul_impls {
    ($name: ident, $dbl: ident) => {
        impl ModMul<$name> for $name {
            fn modmul(&self, x: &$name, m: &$name) -> $name {
                let mulres      = (self as &$name) * x;
                let bigm        = $dbl::from(m);
                let (_, bigres) = mulres.divmod(&bigm);
                $name::from(bigres)
            }
        }
    };
}