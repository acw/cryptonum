addition_impls!(U192, U256);
base_impls!(U192, 3);
multiply_impls!(U192, U384);
conversion_impls!(U192, U256);
conversion_impls!(U192, U384);
addition_impls!(U256, U320);
base_impls!(U256, 4);
multiply_impls!(U256, U512);
subtraction_impls!(U256, 4);
conversion_impls!(U256, U320);
conversion_impls!(U256, U512);
base_impls!(U320, 5);
addition_impls!(U384, U448);
base_impls!(U384, 6);
multiply_impls!(U384, U768);
conversion_impls!(U384, U448);
conversion_impls!(U384, U768);
base_impls!(U448, 7);
conversion_impls!(U448, U512);
base_impls!(U512, 8);
barrett_impl!(BarrettU512, U512, U576, U1024, U1088);
modexp_impls!(U512, U512);
modexp_impls!(U512, BarrettU512);
modmul_impls!(U512, U1024, BarrettU512);
modsq_impls!(U512, U1024, BarrettU512);
multiply_impls!(U512, U1024);
square_impls!(U512, U1024, 512);
subtraction_impls!(U512, 8);
conversion_impls!(U512, U576);
conversion_impls!(U512, U1024);
conversion_impls!(U512, U1088);
addition_impls!(U576, U640);
base_impls!(U576, 9);
multiply_impls!(U576, U1152);
shift_impls!(U576, 9);
subtraction_impls!(U576, 9);
conversion_impls!(U576, U640);
conversion_impls!(U576, U1024);
conversion_impls!(U576, U1088);
conversion_impls!(U576, U1152);
base_impls!(U640, 10);
base_impls!(U768, 12);
base_impls!(U1024, 16);
barrett_impl!(BarrettU1024, U1024, U1088, U2048, U2112);
div_impls!(U1024, U2048);
modexp_impls!(U1024, U1024);
modexp_impls!(U1024, BarrettU1024);
modmul_impls!(U1024, U2048, BarrettU1024);
modsq_impls!(U1024, U2048, BarrettU1024);
multiply_impls!(U1024, U2048);
shift_impls!(U1024, 16);
square_impls!(U1024, U2048, 1024);
subtraction_impls!(U1024, 16);
conversion_impls!(U1024, U1088);
conversion_impls!(U1024, U2048);
conversion_impls!(U1024, U2112);
addition_impls!(U1088, U1152);
base_impls!(U1088, 17);
div_impls!(U1088, U2176);
multiply_impls!(U1088, U2176);
shift_impls!(U1088, 17);
subtraction_impls!(U1088, 17);
conversion_impls!(U1088, U1152);
conversion_impls!(U1088, U2048);
conversion_impls!(U1088, U2112);
conversion_impls!(U1088, U2176);
base_impls!(U1152, 18);
shift_impls!(U1152, 18);
base_impls!(U1536, 24);
multiply_impls!(U1536, U3072);
subtraction_impls!(U1536, 24);
conversion_impls!(U1536, U3072);
base_impls!(U2048, 32);
barrett_impl!(BarrettU2048, U2048, U2112, U4096, U4160);
div_impls!(U2048, U4096);
modexp_impls!(U2048, U2048);
modexp_impls!(U2048, BarrettU2048);
modmul_impls!(U2048, U4096, BarrettU2048);
modsq_impls!(U2048, U4096, BarrettU2048);
multiply_impls!(U2048, U4096);
shift_impls!(U2048, 32);
square_impls!(U2048, U4096, 2048);
subtraction_impls!(U2048, 32);
conversion_impls!(U2048, U2112);
conversion_impls!(U2048, U4096);
conversion_impls!(U2048, U4160);
addition_impls!(U2112, U2176);
base_impls!(U2112, 33);
div_impls!(U2112, U4224);
multiply_impls!(U2112, U4224);
shift_impls!(U2112, 33);
subtraction_impls!(U2112, 33);
conversion_impls!(U2112, U2176);
conversion_impls!(U2112, U4096);
conversion_impls!(U2112, U4160);
conversion_impls!(U2112, U4224);
base_impls!(U2176, 34);
shift_impls!(U2176, 34);
subtraction_impls!(U2176, 34);
base_impls!(U3072, 48);
barrett_impl!(BarrettU3072, U3072, U3136, U6144, U6208);
modexp_impls!(U3072, U3072);
modexp_impls!(U3072, BarrettU3072);
modmul_impls!(U3072, U6144, BarrettU3072);
modsq_impls!(U3072, U6144, BarrettU3072);
multiply_impls!(U3072, U6144);
square_impls!(U3072, U6144, 3072);
conversion_impls!(U3072, U3136);
conversion_impls!(U3072, U6144);
conversion_impls!(U3072, U6208);
addition_impls!(U3136, U3200);
base_impls!(U3136, 49);
multiply_impls!(U3136, U6272);
shift_impls!(U3136, 49);
subtraction_impls!(U3136, 49);
conversion_impls!(U3136, U3200);
conversion_impls!(U3136, U6144);
conversion_impls!(U3136, U6208);
conversion_impls!(U3136, U6272);
base_impls!(U3200, 50);
base_impls!(U4096, 64);
barrett_impl!(BarrettU4096, U4096, U4160, U8192, U8256);
div_impls!(U4096, U8192);
modexp_impls!(U4096, U4096);
modexp_impls!(U4096, BarrettU4096);
modmul_impls!(U4096, U8192, BarrettU4096);
modsq_impls!(U4096, U8192, BarrettU4096);
multiply_impls!(U4096, U8192);
shift_impls!(U4096, 64);
square_impls!(U4096, U8192, 4096);
subtraction_impls!(U4096, 64);
conversion_impls!(U4096, U4160);
conversion_impls!(U4096, U8192);
conversion_impls!(U4096, U8256);
addition_impls!(U4160, U4224);
base_impls!(U4160, 65);
div_impls!(U4160, U8320);
multiply_impls!(U4160, U8320);
shift_impls!(U4160, 65);
subtraction_impls!(U4160, 65);
conversion_impls!(U4160, U4224);
conversion_impls!(U4160, U8192);
conversion_impls!(U4160, U8256);
conversion_impls!(U4160, U8320);
base_impls!(U4224, 66);
shift_impls!(U4224, 66);
subtraction_impls!(U4224, 66);
base_impls!(U6144, 96);
div_impls!(U6144, U12288);
multiply_impls!(U6144, U12288);
shift_impls!(U6144, 96);
conversion_impls!(U6144, U6208);
conversion_impls!(U6144, U12288);
addition_impls!(U6208, U6272);
base_impls!(U6208, 97);
div_impls!(U6208, U12416);
multiply_impls!(U6208, U12416);
subtraction_impls!(U6208, 97);
conversion_impls!(U6208, U6272);
conversion_impls!(U6208, U12416);
base_impls!(U6272, 98);
shift_impls!(U6272, 98);
base_impls!(U7680, 120);
multiply_impls!(U7680, U15360);
subtraction_impls!(U7680, 120);
conversion_impls!(U7680, U15360);
base_impls!(U8192, 128);
barrett_impl!(BarrettU8192, U8192, U8256, U16384, U16448);
div_impls!(U8192, U16384);
modexp_impls!(U8192, U8192);
modexp_impls!(U8192, BarrettU8192);
modmul_impls!(U8192, U16384, BarrettU8192);
modsq_impls!(U8192, U16384, BarrettU8192);
multiply_impls!(U8192, U16384);
shift_impls!(U8192, 128);
square_impls!(U8192, U16384, 8192);
subtraction_impls!(U8192, 128);
conversion_impls!(U8192, U8256);
conversion_impls!(U8192, U16384);
conversion_impls!(U8192, U16448);
addition_impls!(U8256, U8320);
base_impls!(U8256, 129);
div_impls!(U8256, U16512);
multiply_impls!(U8256, U16512);
shift_impls!(U8256, 129);
subtraction_impls!(U8256, 129);
conversion_impls!(U8256, U8320);
conversion_impls!(U8256, U16384);
conversion_impls!(U8256, U16448);
conversion_impls!(U8256, U16512);
base_impls!(U8320, 130);
shift_impls!(U8320, 130);
subtraction_impls!(U8320, 130);
base_impls!(U12288, 192);
subtraction_impls!(U12288, 192);
base_impls!(U12416, 194);
subtraction_impls!(U12416, 194);
base_impls!(U15360, 240);
barrett_impl!(BarrettU15360, U15360, U15424, U30720, U30784);
modexp_impls!(U15360, U15360);
modexp_impls!(U15360, BarrettU15360);
modmul_impls!(U15360, U30720, BarrettU15360);
modsq_impls!(U15360, U30720, BarrettU15360);
multiply_impls!(U15360, U30720);
square_impls!(U15360, U30720, 15360);
conversion_impls!(U15360, U15424);
conversion_impls!(U15360, U30720);
conversion_impls!(U15360, U30784);
addition_impls!(U15424, U15488);
base_impls!(U15424, 241);
multiply_impls!(U15424, U30848);
shift_impls!(U15424, 241);
subtraction_impls!(U15424, 241);
conversion_impls!(U15424, U15488);
conversion_impls!(U15424, U30720);
conversion_impls!(U15424, U30784);
conversion_impls!(U15424, U30848);
base_impls!(U15488, 242);
base_impls!(U16384, 256);
div_impls!(U16384, U32768);
multiply_impls!(U16384, U32768);
shift_impls!(U16384, 256);
subtraction_impls!(U16384, 256);
conversion_impls!(U16384, U16448);
conversion_impls!(U16384, U32768);
addition_impls!(U16448, U16512);
base_impls!(U16448, 257);
div_impls!(U16448, U32896);
multiply_impls!(U16448, U32896);
subtraction_impls!(U16448, 257);
conversion_impls!(U16448, U16512);
conversion_impls!(U16448, U32896);
base_impls!(U16512, 258);
shift_impls!(U16512, 258);
subtraction_impls!(U16512, 258);
base_impls!(U30720, 480);
div_impls!(U30720, U61440);
multiply_impls!(U30720, U61440);
shift_impls!(U30720, 480);
conversion_impls!(U30720, U30784);
conversion_impls!(U30720, U61440);
addition_impls!(U30784, U30848);
base_impls!(U30784, 481);
div_impls!(U30784, U61568);
multiply_impls!(U30784, U61568);
subtraction_impls!(U30784, 481);
conversion_impls!(U30784, U30848);
conversion_impls!(U30784, U61568);
base_impls!(U30848, 482);
shift_impls!(U30848, 482);
base_impls!(U32768, 512);
subtraction_impls!(U32768, 512);
base_impls!(U32896, 514);
subtraction_impls!(U32896, 514);
base_impls!(U61440, 960);
subtraction_impls!(U61440, 960);
base_impls!(U61568, 962);
subtraction_impls!(U61568, 962);


#[cfg(test)]
mod tests {
  mod base {
    use super::super::*;
    use testing::run_test;

    generate_base_tests!(U192, u192);
    generate_base_tests!(U256, u256);
    generate_base_tests!(U320, u320);
    generate_base_tests!(U384, u384);
    generate_base_tests!(U448, u448);
    generate_base_tests!(U512, u512);
    generate_base_tests!(U576, u576);
    generate_base_tests!(U640, u640);
    generate_base_tests!(U768, u768);
    generate_base_tests!(U1024, u1024);
    generate_base_tests!(U1088, u1088);
    generate_base_tests!(U1152, u1152);
    generate_base_tests!(U1536, u1536);
    generate_base_tests!(U2048, u2048);
    generate_base_tests!(U2112, u2112);
    generate_base_tests!(U2176, u2176);
    generate_base_tests!(U3072, u3072);
    generate_base_tests!(U3136, u3136);
    generate_base_tests!(U3200, u3200);
    generate_base_tests!(U4096, u4096);
    generate_base_tests!(U4160, u4160);
    generate_base_tests!(U4224, u4224);
    generate_base_tests!(U6144, u6144);
    generate_base_tests!(U6208, u6208);
    generate_base_tests!(U6272, u6272);
    generate_base_tests!(U7680, u7680);
    generate_base_tests!(U8192, u8192);
    generate_base_tests!(U8256, u8256);
    generate_base_tests!(U8320, u8320);
    generate_base_tests!(U12288, u12288);
    generate_base_tests!(U12416, u12416);
    generate_base_tests!(U15360, u15360);
    generate_base_tests!(U15424, u15424);
    generate_base_tests!(U15488, u15488);
    generate_base_tests!(ignore U16384, u16384);
    generate_base_tests!(ignore U16448, u16448);
    generate_base_tests!(ignore U16512, u16512);
    generate_base_tests!(ignore U30720, u30720);
    generate_base_tests!(ignore U30784, u30784);
    generate_base_tests!(ignore U30848, u30848);
    generate_base_tests!(ignore U32768, u32768);
    generate_base_tests!(ignore U32896, u32896);
    generate_base_tests!(ignore U61440, u61440);
    generate_base_tests!(ignore U61568, u61568);
  }
  mod conversion {
    generate_conversion_tests!(U192, u192);
    generate_conversion_tests!(U256, u256);
    generate_conversion_tests!(U320, u320);
    generate_conversion_tests!(U384, u384);
    generate_conversion_tests!(U448, u448);
    generate_conversion_tests!(U512, u512);
    generate_conversion_tests!(U576, u576);
    generate_conversion_tests!(U640, u640);
    generate_conversion_tests!(U768, u768);
    generate_conversion_tests!(U1024, u1024);
    generate_conversion_tests!(U1088, u1088);
    generate_conversion_tests!(U1152, u1152);
    generate_conversion_tests!(U1536, u1536);
    generate_conversion_tests!(U2048, u2048);
    generate_conversion_tests!(U2112, u2112);
    generate_conversion_tests!(U2176, u2176);
    generate_conversion_tests!(U3072, u3072);
    generate_conversion_tests!(U3136, u3136);
    generate_conversion_tests!(U3200, u3200);
    generate_conversion_tests!(U4096, u4096);
    generate_conversion_tests!(U4160, u4160);
    generate_conversion_tests!(U4224, u4224);
    generate_conversion_tests!(U6144, u6144);
    generate_conversion_tests!(U6208, u6208);
    generate_conversion_tests!(U6272, u6272);
    generate_conversion_tests!(U7680, u7680);
    generate_conversion_tests!(U8192, u8192);
    generate_conversion_tests!(U8256, u8256);
    generate_conversion_tests!(U8320, u8320);
    generate_conversion_tests!(U12288, u12288);
    generate_conversion_tests!(U12416, u12416);
    generate_conversion_tests!(U15360, u15360);
    generate_conversion_tests!(U15424, u15424);
    generate_conversion_tests!(U15488, u15488);
    generate_conversion_tests!(U16384, u16384);
    generate_conversion_tests!(U16448, u16448);
    generate_conversion_tests!(U16512, u16512);
    generate_conversion_tests!(U30720, u30720);
    generate_conversion_tests!(U30784, u30784);
    generate_conversion_tests!(U30848, u30848);
    generate_conversion_tests!(U32768, u32768);
    generate_conversion_tests!(U32896, u32896);
    generate_conversion_tests!(U61440, u61440);
    generate_conversion_tests!(U61568, u61568);
  }
  mod codec {
    generate_codec_tests!(U192, u192);
    generate_codec_tests!(U256, u256);
    generate_codec_tests!(U320, u320);
    generate_codec_tests!(U384, u384);
    generate_codec_tests!(U448, u448);
    generate_codec_tests!(U512, u512);
    generate_codec_tests!(U576, u576);
    generate_codec_tests!(U640, u640);
    generate_codec_tests!(U768, u768);
    generate_codec_tests!(U1024, u1024);
    generate_codec_tests!(U1088, u1088);
    generate_codec_tests!(U1152, u1152);
    generate_codec_tests!(U1536, u1536);
    generate_codec_tests!(U2048, u2048);
    generate_codec_tests!(U2112, u2112);
    generate_codec_tests!(U2176, u2176);
    generate_codec_tests!(U3072, u3072);
    generate_codec_tests!(U3136, u3136);
    generate_codec_tests!(U3200, u3200);
    generate_codec_tests!(U4096, u4096);
    generate_codec_tests!(U4160, u4160);
    generate_codec_tests!(U4224, u4224);
    generate_codec_tests!(U6144, u6144);
    generate_codec_tests!(U6208, u6208);
    generate_codec_tests!(U6272, u6272);
    generate_codec_tests!(U7680, u7680);
    generate_codec_tests!(U8192, u8192);
    generate_codec_tests!(U8256, u8256);
    generate_codec_tests!(U8320, u8320);
    generate_codec_tests!(U12288, u12288);
    generate_codec_tests!(U12416, u12416);
    generate_codec_tests!(U15360, u15360);
    generate_codec_tests!(U15424, u15424);
    generate_codec_tests!(U15488, u15488);
    generate_codec_tests!(U16384, u16384);
    generate_codec_tests!(U16448, u16448);
    generate_codec_tests!(U16512, u16512);
    generate_codec_tests!(U30720, u30720);
    generate_codec_tests!(U30784, u30784);
    generate_codec_tests!(U30848, u30848);
    generate_codec_tests!(U32768, u32768);
    generate_codec_tests!(U32896, u32896);
    generate_codec_tests!(U61440, u61440);
    generate_codec_tests!(U61568, u61568);
  }
  mod cmp {
    use super::super::*;
    use testing::run_test;

    generate_cmp_tests!(U192, u192);
    generate_cmp_tests!(U256, u256);
    generate_cmp_tests!(U320, u320);
    generate_cmp_tests!(U384, u384);
    generate_cmp_tests!(U448, u448);
    generate_cmp_tests!(U512, u512);
    generate_cmp_tests!(U576, u576);
    generate_cmp_tests!(U640, u640);
    generate_cmp_tests!(U768, u768);
    generate_cmp_tests!(U1024, u1024);
    generate_cmp_tests!(U1088, u1088);
    generate_cmp_tests!(U1152, u1152);
    generate_cmp_tests!(U1536, u1536);
    generate_cmp_tests!(U2048, u2048);
    generate_cmp_tests!(U2112, u2112);
    generate_cmp_tests!(U2176, u2176);
    generate_cmp_tests!(U3072, u3072);
    generate_cmp_tests!(U3136, u3136);
    generate_cmp_tests!(U3200, u3200);
    generate_cmp_tests!(U4096, u4096);
    generate_cmp_tests!(U4160, u4160);
    generate_cmp_tests!(U4224, u4224);
    generate_cmp_tests!(U6144, u6144);
    generate_cmp_tests!(U6208, u6208);
    generate_cmp_tests!(U6272, u6272);
    generate_cmp_tests!(U7680, u7680);
    generate_cmp_tests!(U8192, u8192);
    generate_cmp_tests!(U8256, u8256);
    generate_cmp_tests!(U8320, u8320);
    generate_cmp_tests!(U12288, u12288);
    generate_cmp_tests!(U12416, u12416);
    generate_cmp_tests!(U15360, u15360);
    generate_cmp_tests!(U15424, u15424);
    generate_cmp_tests!(U15488, u15488);
    generate_cmp_tests!(ignore U16384, u16384);
    generate_cmp_tests!(ignore U16448, u16448);
    generate_cmp_tests!(ignore U16512, u16512);
    generate_cmp_tests!(ignore U30720, u30720);
    generate_cmp_tests!(ignore U30784, u30784);
    generate_cmp_tests!(ignore U30848, u30848);
    generate_cmp_tests!(ignore U32768, u32768);
    generate_cmp_tests!(ignore U32896, u32896);
    generate_cmp_tests!(ignore U61440, u61440);
    generate_cmp_tests!(ignore U61568, u61568);
  }
  mod sub {
    use super::super::*;
    use testing::run_test;

    generate_sub_tests!(U256, u256);
    generate_sub_tests!(U512, u512);
    generate_sub_tests!(U576, u576);
    generate_sub_tests!(U1024, u1024);
    generate_sub_tests!(U1088, u1088);
    generate_sub_tests!(U1536, u1536);
    generate_sub_tests!(U2048, u2048);
    generate_sub_tests!(U2112, u2112);
    generate_sub_tests!(U2176, u2176);
    generate_sub_tests!(U3136, u3136);
    generate_sub_tests!(U4096, u4096);
    generate_sub_tests!(U4160, u4160);
    generate_sub_tests!(U4224, u4224);
    generate_sub_tests!(U6208, u6208);
    generate_sub_tests!(U7680, u7680);
    generate_sub_tests!(U8192, u8192);
    generate_sub_tests!(U8256, u8256);
    generate_sub_tests!(U8320, u8320);
    generate_sub_tests!(ignore U12288, u12288);
    generate_sub_tests!(ignore U12416, u12416);
    generate_sub_tests!(ignore U15424, u15424);
    generate_sub_tests!(ignore U16384, u16384);
    generate_sub_tests!(ignore U16448, u16448);
    generate_sub_tests!(ignore U16512, u16512);
    generate_sub_tests!(ignore U30784, u30784);
    generate_sub_tests!(ignore U32768, u32768);
    generate_sub_tests!(ignore U32896, u32896);
    generate_sub_tests!(ignore U61440, u61440);
    generate_sub_tests!(ignore U61568, u61568);
  }
  mod shiftl {
    use super::super::*;
    use testing::run_test;

    generate_shiftl_tests!(U576, u576);
    generate_shiftl_tests!(U1024, u1024);
    generate_shiftl_tests!(U1088, u1088);
    generate_shiftl_tests!(U1152, u1152);
    generate_shiftl_tests!(U2048, u2048);
    generate_shiftl_tests!(U2112, u2112);
    generate_shiftl_tests!(U2176, u2176);
    generate_shiftl_tests!(U3136, u3136);
    generate_shiftl_tests!(U4096, u4096);
    generate_shiftl_tests!(U4160, u4160);
    generate_shiftl_tests!(U4224, u4224);
    generate_shiftl_tests!(U6144, u6144);
    generate_shiftl_tests!(U6272, u6272);
    generate_shiftl_tests!(U8192, u8192);
    generate_shiftl_tests!(U8256, u8256);
    generate_shiftl_tests!(U8320, u8320);
    generate_shiftl_tests!(ignore U15424, u15424);
    generate_shiftl_tests!(ignore U16384, u16384);
    generate_shiftl_tests!(ignore U16512, u16512);
    generate_shiftl_tests!(ignore U30720, u30720);
    generate_shiftl_tests!(ignore U30848, u30848);
  }
  mod shiftr {
    use super::super::*;
    use testing::run_test;

    generate_shiftr_tests!(U576, u576);
    generate_shiftr_tests!(U1024, u1024);
    generate_shiftr_tests!(U1088, u1088);
    generate_shiftr_tests!(U1152, u1152);
    generate_shiftr_tests!(U2048, u2048);
    generate_shiftr_tests!(U2112, u2112);
    generate_shiftr_tests!(U2176, u2176);
    generate_shiftr_tests!(U3136, u3136);
    generate_shiftr_tests!(U4096, u4096);
    generate_shiftr_tests!(U4160, u4160);
    generate_shiftr_tests!(U4224, u4224);
    generate_shiftr_tests!(U6144, u6144);
    generate_shiftr_tests!(U6272, u6272);
    generate_shiftr_tests!(U8192, u8192);
    generate_shiftr_tests!(U8256, u8256);
    generate_shiftr_tests!(U8320, u8320);
    generate_shiftr_tests!(ignore U15424, u15424);
    generate_shiftr_tests!(ignore U16384, u16384);
    generate_shiftr_tests!(ignore U16512, u16512);
    generate_shiftr_tests!(ignore U30720, u30720);
    generate_shiftr_tests!(ignore U30848, u30848);
  }
  mod add {
    use super::super::*;
    use testing::run_test;

    generate_add_tests!(U192, u192, U256);
    generate_add_tests!(U256, u256, U320);
    generate_add_tests!(U384, u384, U448);
    generate_add_tests!(U576, u576, U640);
    generate_add_tests!(U1088, u1088, U1152);
    generate_add_tests!(U2112, u2112, U2176);
    generate_add_tests!(U3136, u3136, U3200);
    generate_add_tests!(U4160, u4160, U4224);
    generate_add_tests!(U6208, u6208, U6272);
    generate_add_tests!(U8256, u8256, U8320);
    generate_add_tests!(ignore U15424, u15424, U15488);
    generate_add_tests!(ignore U16448, u16448, U16512);
    generate_add_tests!(ignore U30784, u30784, U30848);
  }
  mod mul {
    use super::super::*;
    use testing::run_test;

    generate_mul_tests!(U192, u192, U384);
    generate_mul_tests!(U256, u256, U512);
    generate_mul_tests!(U384, u384, U768);
    generate_mul_tests!(U512, u512, U1024);
    generate_mul_tests!(U576, u576, U1152);
    generate_mul_tests!(U1024, u1024, U2048);
    generate_mul_tests!(U1088, u1088, U2176);
    generate_mul_tests!(U1536, u1536, U3072);
    generate_mul_tests!(U2048, u2048, U4096);
    generate_mul_tests!(U2112, u2112, U4224);
    generate_mul_tests!(U3072, u3072, U6144);
    generate_mul_tests!(U3136, u3136, U6272);
    generate_mul_tests!(U4096, u4096, U8192);
    generate_mul_tests!(U4160, u4160, U8320);
    generate_mul_tests!(U6144, u6144, U12288);
    generate_mul_tests!(U6208, u6208, U12416);
    generate_mul_tests!(U7680, u7680, U15360);
    generate_mul_tests!(U8192, u8192, U16384);
    generate_mul_tests!(U8256, u8256, U16512);
    generate_mul_tests!(ignore U15360, u15360, U30720);
    generate_mul_tests!(ignore U15424, u15424, U30848);
    generate_mul_tests!(ignore U16384, u16384, U32768);
    generate_mul_tests!(ignore U16448, u16448, U32896);
    generate_mul_tests!(ignore U30720, u30720, U61440);
    generate_mul_tests!(ignore U30784, u30784, U61568);
  }
  mod div {
    use super::super::*;
    use testing::run_test;

    generate_div_tests!(U1024, u1024);
    generate_div_tests!(U1088, u1088);
    generate_div_tests!(U2048, u2048);
    generate_div_tests!(ignore U2112, u2112);
    generate_div_tests!(ignore U4096, u4096);
    generate_div_tests!(ignore U4160, u4160);
    generate_div_tests!(ignore U6144, u6144);
    generate_div_tests!(ignore U6208, u6208);
    generate_div_tests!(ignore U8192, u8192);
    generate_div_tests!(ignore U8256, u8256);
    generate_div_tests!(ignore U16384, u16384);
    generate_div_tests!(ignore U16448, u16448);
    generate_div_tests!(ignore U30720, u30720);
    generate_div_tests!(ignore U30784, u30784);
  }
  mod barrett_gen {
    use super::super::*;
    use testing::run_test;

    generate_barrett_gen_tests!(U512, u512, U576);
    generate_barrett_gen_tests!(U1024, u1024, U1088);
    generate_barrett_gen_tests!(ignore U2048, u2048, U2112);
    generate_barrett_gen_tests!(ignore U3072, u3072, U3136);
    generate_barrett_gen_tests!(ignore U4096, u4096, U4160);
    generate_barrett_gen_tests!(ignore U8192, u8192, U8256);
    generate_barrett_gen_tests!(ignore U15360, u15360, U15424);
  }
  mod barrett_red {
    use super::super::*;
    use testing::run_test;

    generate_barrett_red_tests!(U512, u512, U576, U1024);
    generate_barrett_red_tests!(U1024, u1024, U1088, U2048);
    generate_barrett_red_tests!(U2048, u2048, U2112, U4096);
    generate_barrett_red_tests!(U3072, u3072, U3136, U6144);
    generate_barrett_red_tests!(ignore U4096, u4096, U4160, U8192);
    generate_barrett_red_tests!(ignore U8192, u8192, U8256, U16384);
    generate_barrett_red_tests!(ignore U15360, u15360, U15424, U30720);
  }
  mod modsq {
    use super::super::*;
    use testing::run_test;

    generate_modsq_tests!(U512, u512);
    generate_modsq_tests!(U1024, u1024);
    generate_modsq_tests!(U2048, u2048);
    generate_modsq_tests!(U3072, u3072);
    generate_modsq_tests!(ignore U4096, u4096);
    generate_modsq_tests!(ignore U8192, u8192);
    generate_modsq_tests!(ignore U15360, u15360);
  }
  mod modmul {
    use super::super::*;
    use testing::run_test;

    generate_modmul_tests!(U512, u512);
    generate_modmul_tests!(U1024, u1024);
    generate_modmul_tests!(U2048, u2048);
    generate_modmul_tests!(U3072, u3072);
    generate_modmul_tests!(ignore U4096, u4096);
    generate_modmul_tests!(ignore U8192, u8192);
    generate_modmul_tests!(ignore U15360, u15360);
  }
  mod modexp {
    use super::super::*;
    use testing::run_test;

    generate_modexp_tests!(ignore U512, u512);
    generate_modexp_tests!(ignore U1024, u1024);
    generate_modexp_tests!(ignore U2048, u2048);
    generate_modexp_tests!(ignore U3072, u3072);
    generate_modexp_tests!(ignore U4096, u4096);
    generate_modexp_tests!(ignore U8192, u8192);
    generate_modexp_tests!(ignore U15360, u15360);
  }
  mod square {
    use super::super::*;
    use testing::run_test;

    generate_square_tests!(U512, u512, U1024);
    generate_square_tests!(U1024, u1024, U2048);
    generate_square_tests!(U2048, u2048, U4096);
    generate_square_tests!(U3072, u3072, U6144);
    generate_square_tests!(ignore U4096, u4096, U8192);
    generate_square_tests!(ignore U8192, u8192, U16384);
    generate_square_tests!(ignore U15360, u15360, U30720);
  }
  mod barrett_modsq {
    use super::super::*;
    use testing::run_test;

    generate_barrett_modsq_tests!(U512, u512, U576);
    generate_barrett_modsq_tests!(U1024, u1024, U1088);
    generate_barrett_modsq_tests!(U2048, u2048, U2112);
    generate_barrett_modsq_tests!(U3072, u3072, U3136);
    generate_barrett_modsq_tests!(ignore U4096, u4096, U4160);
    generate_barrett_modsq_tests!(ignore U8192, u8192, U8256);
    generate_barrett_modsq_tests!(ignore U15360, u15360, U15424);
  }
  mod barrett_modmul {
    use super::super::*;
    use testing::run_test;

    generate_barrett_modmul_tests!(U512, u512, U576);
    generate_barrett_modmul_tests!(U1024, u1024, U1088);
    generate_barrett_modmul_tests!(U2048, u2048, U2112);
    generate_barrett_modmul_tests!(U3072, u3072, U3136);
    generate_barrett_modmul_tests!(ignore U4096, u4096, U4160);
    generate_barrett_modmul_tests!(ignore U8192, u8192, U8256);
    generate_barrett_modmul_tests!(ignore U15360, u15360, U15424);
  }
  mod barrett_modexp {
    use super::super::*;
    use testing::run_test;

    generate_barrett_modexp_tests!(U512, u512, U576);
    generate_barrett_modexp_tests!(ignore U1024, u1024, U1088);
    generate_barrett_modexp_tests!(ignore U2048, u2048, U2112);
    generate_barrett_modexp_tests!(ignore U3072, u3072, U3136);
    generate_barrett_modexp_tests!(ignore U4096, u4096, U4160);
    generate_barrett_modexp_tests!(ignore U8192, u8192, U8256);
    generate_barrett_modexp_tests!(ignore U15360, u15360, U15424);
  }
}
