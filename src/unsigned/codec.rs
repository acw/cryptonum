/// Conversion from bytes into the numeric type.
pub trait Decoder {
    fn from_bytes(x: &[u8]) -> Self;
}

/// Conversion from the numeric types into a byte buffer.
pub trait Encoder {
    fn to_bytes(&self) -> Vec<u8>;
}

pub(crate) fn raw_decoder(input: &[u8], output: &mut [u64])
{
    let mut item = 0;
    let mut shift = 0;
    let mut idx = 0;

    for v in input.iter().rev() {
        item |= (*v as u64) << shift;
        shift += 8;
        if shift == 64 {
            shift = 0;
            output[idx] = item;
            idx += 1;
            item = 0;
        }
    }
    if item != 0 {
        output[idx] = item;
    }
}

macro_rules! generate_decoder {
    ($name: ident) => {
        impl Decoder for $name {
            fn from_bytes(x: &[u8]) -> $name {
                let mut res = $name::zero();
                raw_decoder(x, &mut res.value);
                res
            }
        }
    }
}

macro_rules! generate_encoder {
    ($name: ident) => {
        impl Encoder for $name {
            fn to_bytes(&self) -> Vec<u8> {
                let mut res = Vec::with_capacity(self.value.len() * 8);
                for v in self.value.iter().rev() {
                    let val = *v;
                    res.push( (val >> 56) as u8);
                    res.push( (val >> 48) as u8);
                    res.push( (val >> 40) as u8);
                    res.push( (val >> 32) as u8);
                    res.push( (val >> 24) as u8);
                    res.push( (val >> 16) as u8);
                    res.push( (val >>  8) as u8);
                    res.push( (val >>  0) as u8);
                }
                res
            }
        }
    }
}

macro_rules! generate_codec
{
    ($name: ident) => {
        generate_decoder!($name);
        generate_encoder!($name);
    }
}

#[cfg(test)]
macro_rules! generate_codec_tests {
    ($name: ident, $lname: ident) => {
        #[cfg(test)]
        mod $lname {
            use super::super::super::*;

            quickcheck! {
                fn decode_encode(x: $name) -> bool {
                    let bytes = x.to_bytes();
                    let x2    = $name::from_bytes(&bytes);
                    x == x2
                }
            }
        }
    };
}