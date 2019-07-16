macro_rules! generate_formatter {
    ($name: ident) => {
        impl fmt::UpperHex for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                for x in self.value.iter().rev() {
                    f.write_char(tochar(true, x >> 60))?;
                    f.write_char(tochar(true, x >> 56))?;
                    f.write_char(tochar(true, x >> 52))?;
                    f.write_char(tochar(true, x >> 48))?;
                    f.write_char(tochar(true, x >> 44))?;
                    f.write_char(tochar(true, x >> 40))?;
                    f.write_char(tochar(true, x >> 36))?;
                    f.write_char(tochar(true, x >> 32))?;
                    f.write_char(tochar(true, x >> 28))?;
                    f.write_char(tochar(true, x >> 24))?;
                    f.write_char(tochar(true, x >> 20))?;
                    f.write_char(tochar(true, x >> 16))?;
                    f.write_char(tochar(true, x >> 12))?;
                    f.write_char(tochar(true, x >>  8))?;
                    f.write_char(tochar(true, x >>  4))?;
                    f.write_char(tochar(true, x >>  0))?;
                }
                Ok(())
            }
        }

        impl fmt::LowerHex for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                for x in self.value.iter().rev() {
                    f.write_char(tochar(false, x >> 60))?;
                    f.write_char(tochar(false, x >> 56))?;
                    f.write_char(tochar(false, x >> 52))?;
                    f.write_char(tochar(false, x >> 48))?;
                    f.write_char(tochar(false, x >> 44))?;
                    f.write_char(tochar(false, x >> 40))?;
                    f.write_char(tochar(false, x >> 36))?;
                    f.write_char(tochar(false, x >> 32))?;
                    f.write_char(tochar(false, x >> 28))?;
                    f.write_char(tochar(false, x >> 24))?;
                    f.write_char(tochar(false, x >> 20))?;
                    f.write_char(tochar(false, x >> 16))?;
                    f.write_char(tochar(false, x >> 12))?;
                    f.write_char(tochar(false, x >>  8))?;
                    f.write_char(tochar(false, x >>  4))?;
                    f.write_char(tochar(false, x >>  0))?;
                }
                Ok(())
            }
        }
    };
}

pub fn tochar(upper: bool, val: u64) -> char {
    match val & 0xF {
        0x0 => '0',
        0x1 => '1',
        0x2 => '2',
        0x3 => '3',
        0x4 => '4',
        0x5 => '5',
        0x6 => '6',
        0x7 => '7',
        0x8 => '8',
        0x9 => '9',
        0xA => if upper { 'A' } else { 'a' },
        0xB => if upper { 'B' } else { 'b' },
        0xC => if upper { 'C' } else { 'c' },
        0xD => if upper { 'D' } else { 'd' },
        0xE => if upper { 'E' } else { 'e' },
        0xF => if upper { 'F' } else { 'f' },
        _   => panic!("The world is broken, rejoice, rejoice.")
    }
}