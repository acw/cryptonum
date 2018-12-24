signed_impls!(I192, U192);
subtraction_impls!(I192, I256, U256);
conversion_impls!(I192, U192, I256, U256);
conversion_impls!(I192, U192, I512, U512);
egcd_impls!(I256, U192, I192);
modinv_impls!(U192, I256, U256);
add_impls!(I256, I320, U320);
signed_impls!(I256, U256);
cmp_impls!(I256);
shift_impls!(I256, U256);
subtraction_impls!(I256, I320, U320);
mul_impls!(I256, I512);
conversion_impls!(I256, U256, I320, U320);
conversion_impls!(I256, U256, I512, U512);
conversion_impls!(I256, U256, I640, U640);
egcd_impls!(I320, U256, I256);
modinv_impls!(U256, I320, U320);
add_impls!(I320, I384, U384);
signed_impls!(I320, U320);
cmp_impls!(I320);
shift_impls!(I320, U320);
subtraction_impls!(I320, I384, U384);
mul_impls!(I320, I640);
conversion_impls!(I320, U320, I384, U384);
conversion_impls!(I320, U320, I640, U640);
signed_impls!(I384, U384);
subtraction_impls!(I384, I448, U448);
conversion_impls!(I384, U384, I448, U448);
conversion_impls!(I384, U384, I896, U896);
signed_impls!(I448, U448);
mul_impls!(I448, I896);
conversion_impls!(I448, U448, I896, U896);
signed_impls!(I512, U512);
subtraction_impls!(I512, I576, U576);
div_impls!(I512, U512);
conversion_impls!(I512, U512, I576, U576);
egcd_impls!(I576, U512, I512);
modinv_impls!(U512, I576, U576);
add_impls!(I576, I640, U640);
signed_impls!(I576, U576);
cmp_impls!(I576);
shift_impls!(I576, U576);
subtraction_impls!(I576, I640, U640);
conversion_impls!(I576, U576, I640, U640);
conversion_impls!(I576, U576, I1280, U1280);
signed_impls!(I640, U640);
subtraction_impls!(I640, I704, U704);
mul_impls!(I640, I1280);
div_impls!(I640, U640);
conversion_impls!(I640, U640, I704, U704);
conversion_impls!(I640, U640, I1280, U1280);
signed_impls!(I704, U704);
signed_impls!(I896, U896);
subtraction_impls!(I896, I960, U960);
div_impls!(I896, U896);
conversion_impls!(I896, U896, I960, U960);
signed_impls!(I960, U960);
signed_impls!(I1024, U1024);
conversion_impls!(I1024, U1024, I1088, U1088);
egcd_impls!(I1088, U1024, I1024);
modinv_impls!(U1024, I1088, U1088);
add_impls!(I1088, I1152, U1152);
signed_impls!(I1088, U1088);
cmp_impls!(I1088);
shift_impls!(I1088, U1088);
subtraction_impls!(I1088, I1152, U1152);
conversion_impls!(I1088, U1088, I1152, U1152);
signed_impls!(I1152, U1152);
signed_impls!(I1280, U1280);
subtraction_impls!(I1280, I1344, U1344);
div_impls!(I1280, U1280);
conversion_impls!(I1280, U1280, I1344, U1344);
signed_impls!(I1344, U1344);
signed_impls!(I1536, U1536);
conversion_impls!(I1536, U1536, I1600, U1600);
egcd_impls!(I1600, U1536, I1536);
add_impls!(I1600, I1664, U1664);
signed_impls!(I1600, U1600);
cmp_impls!(I1600);
shift_impls!(I1600, U1600);
subtraction_impls!(I1600, I1664, U1664);
conversion_impls!(I1600, U1600, I1664, U1664);
signed_impls!(I1664, U1664);
signed_impls!(I2048, U2048);
conversion_impls!(I2048, U2048, I2112, U2112);
egcd_impls!(I2112, U2048, I2048);
modinv_impls!(U2048, I2112, U2112);
add_impls!(I2112, I2176, U2176);
signed_impls!(I2112, U2112);
cmp_impls!(I2112);
shift_impls!(I2112, U2112);
subtraction_impls!(I2112, I2176, U2176);
conversion_impls!(I2112, U2112, I2176, U2176);
signed_impls!(I2176, U2176);
signed_impls!(I3072, U3072);
conversion_impls!(I3072, U3072, I3136, U3136);
egcd_impls!(I3136, U3072, I3072);
modinv_impls!(U3072, I3136, U3136);
add_impls!(I3136, I3200, U3200);
signed_impls!(I3136, U3136);
cmp_impls!(I3136);
shift_impls!(I3136, U3136);
subtraction_impls!(I3136, I3200, U3200);
conversion_impls!(I3136, U3136, I3200, U3200);
signed_impls!(I3200, U3200);
signed_impls!(I4096, U4096);
conversion_impls!(I4096, U4096, I4160, U4160);
egcd_impls!(I4160, U4096, I4096);
modinv_impls!(U4096, I4160, U4160);
add_impls!(I4160, I4224, U4224);
signed_impls!(I4160, U4160);
cmp_impls!(I4160);
shift_impls!(I4160, U4160);
subtraction_impls!(I4160, I4224, U4224);
conversion_impls!(I4160, U4160, I4224, U4224);
signed_impls!(I4224, U4224);
signed_impls!(I7680, U7680);
conversion_impls!(I7680, U7680, I7744, U7744);
egcd_impls!(I7744, U7680, I7680);
add_impls!(I7744, I7808, U7808);
signed_impls!(I7744, U7744);
cmp_impls!(I7744);
shift_impls!(I7744, U7744);
subtraction_impls!(I7744, I7808, U7808);
conversion_impls!(I7744, U7744, I7808, U7808);
signed_impls!(I7808, U7808);
signed_impls!(I8192, U8192);
conversion_impls!(I8192, U8192, I8256, U8256);
egcd_impls!(I8256, U8192, I8192);
modinv_impls!(U8192, I8256, U8256);
add_impls!(I8256, I8320, U8320);
signed_impls!(I8256, U8256);
cmp_impls!(I8256);
shift_impls!(I8256, U8256);
subtraction_impls!(I8256, I8320, U8320);
conversion_impls!(I8256, U8256, I8320, U8320);
signed_impls!(I8320, U8320);
signed_impls!(I15360, U15360);
conversion_impls!(I15360, U15360, I15424, U15424);
egcd_impls!(I15424, U15360, I15360);
modinv_impls!(U15360, I15424, U15424);
add_impls!(I15424, I15488, U15488);
signed_impls!(I15424, U15424);
cmp_impls!(I15424);
shift_impls!(I15424, U15424);
subtraction_impls!(I15424, I15488, U15488);
conversion_impls!(I15424, U15424, I15488, U15488);
signed_impls!(I15488, U15488);


#[cfg(test)]
mod tests {
  mod sigadd {
    use super::super::*;
    use testing::{build_test_path,run_test};

    generate_sigadd_tests!(I256, U256, i256, I320, U320);
    generate_sigadd_tests!(I320, U320, i320, I384, U384);
    generate_sigadd_tests!(I576, U576, i576, I640, U640);
    generate_sigadd_tests!(I1088, U1088, i1088, I1152, U1152);
    generate_sigadd_tests!(I1600, U1600, i1600, I1664, U1664);
    generate_sigadd_tests!(I2112, U2112, i2112, I2176, U2176);
    generate_sigadd_tests!(I3136, U3136, i3136, I3200, U3200);
    generate_sigadd_tests!(I4160, U4160, i4160, I4224, U4224);
    generate_sigadd_tests!(I7744, U7744, i7744, I7808, U7808);
    generate_sigadd_tests!(I8256, U8256, i8256, I8320, U8320);
    generate_sigadd_tests!(I15424, U15424, i15424, I15488, U15488);
  }
  mod sigsub {
    use super::super::*;
    use testing::{build_test_path,run_test};

    generate_sigsub_tests!(I192, U192, i192, I256, U256);
    generate_sigsub_tests!(I256, U256, i256, I320, U320);
    generate_sigsub_tests!(I320, U320, i320, I384, U384);
    generate_sigsub_tests!(I384, U384, i384, I448, U448);
    generate_sigsub_tests!(I512, U512, i512, I576, U576);
    generate_sigsub_tests!(I576, U576, i576, I640, U640);
    generate_sigsub_tests!(I640, U640, i640, I704, U704);
    generate_sigsub_tests!(I896, U896, i896, I960, U960);
    generate_sigsub_tests!(I1088, U1088, i1088, I1152, U1152);
    generate_sigsub_tests!(I1280, U1280, i1280, I1344, U1344);
    generate_sigsub_tests!(I1600, U1600, i1600, I1664, U1664);
    generate_sigsub_tests!(I2112, U2112, i2112, I2176, U2176);
    generate_sigsub_tests!(I3136, U3136, i3136, I3200, U3200);
    generate_sigsub_tests!(I4160, U4160, i4160, I4224, U4224);
    generate_sigsub_tests!(I7744, U7744, i7744, I7808, U7808);
    generate_sigsub_tests!(I8256, U8256, i8256, I8320, U8320);
    generate_sigsub_tests!(I15424, U15424, i15424, I15488, U15488);
  }
  mod signed {
    use super::super::*;
    use testing::{build_test_path,run_test};

    generate_signed_tests!(I192, U192, i192);
    generate_signed_tests!(I256, U256, i256);
    generate_signed_tests!(I320, U320, i320);
    generate_signed_tests!(I384, U384, i384);
    generate_signed_tests!(I448, U448, i448);
    generate_signed_tests!(I512, U512, i512);
    generate_signed_tests!(I576, U576, i576);
    generate_signed_tests!(I640, U640, i640);
    generate_signed_tests!(I704, U704, i704);
    generate_signed_tests!(I896, U896, i896);
    generate_signed_tests!(I960, U960, i960);
    generate_signed_tests!(I1024, U1024, i1024);
    generate_signed_tests!(I1088, U1088, i1088);
    generate_signed_tests!(I1152, U1152, i1152);
    generate_signed_tests!(I1280, U1280, i1280);
    generate_signed_tests!(I1344, U1344, i1344);
    generate_signed_tests!(I1536, U1536, i1536);
    generate_signed_tests!(I1600, U1600, i1600);
    generate_signed_tests!(I1664, U1664, i1664);
    generate_signed_tests!(I2048, U2048, i2048);
    generate_signed_tests!(I2112, U2112, i2112);
    generate_signed_tests!(I2176, U2176, i2176);
    generate_signed_tests!(I3072, U3072, i3072);
    generate_signed_tests!(I3136, U3136, i3136);
    generate_signed_tests!(I3200, U3200, i3200);
    generate_signed_tests!(I4096, U4096, i4096);
    generate_signed_tests!(I4160, U4160, i4160);
    generate_signed_tests!(I4224, U4224, i4224);
    generate_signed_tests!(I7680, U7680, i7680);
    generate_signed_tests!(I7744, U7744, i7744);
    generate_signed_tests!(I7808, U7808, i7808);
    generate_signed_tests!(I8192, U8192, i8192);
    generate_signed_tests!(I8256, U8256, i8256);
    generate_signed_tests!(I8320, U8320, i8320);
    generate_signed_tests!(I15360, U15360, i15360);
    generate_signed_tests!(I15424, U15424, i15424);
    generate_signed_tests!(I15488, U15488, i15488);
  }
  mod sigconversion {
    generate_sigconversion_tests!(I192, U192, i192);
    generate_sigconversion_tests!(I256, U256, i256);
    generate_sigconversion_tests!(I320, U320, i320);
    generate_sigconversion_tests!(I384, U384, i384);
    generate_sigconversion_tests!(I448, U448, i448);
    generate_sigconversion_tests!(I512, U512, i512);
    generate_sigconversion_tests!(I576, U576, i576);
    generate_sigconversion_tests!(I640, U640, i640);
    generate_sigconversion_tests!(I704, U704, i704);
    generate_sigconversion_tests!(I896, U896, i896);
    generate_sigconversion_tests!(I960, U960, i960);
    generate_sigconversion_tests!(I1024, U1024, i1024);
    generate_sigconversion_tests!(I1088, U1088, i1088);
    generate_sigconversion_tests!(I1152, U1152, i1152);
    generate_sigconversion_tests!(I1280, U1280, i1280);
    generate_sigconversion_tests!(I1344, U1344, i1344);
    generate_sigconversion_tests!(I1536, U1536, i1536);
    generate_sigconversion_tests!(I1600, U1600, i1600);
    generate_sigconversion_tests!(I1664, U1664, i1664);
    generate_sigconversion_tests!(I2048, U2048, i2048);
    generate_sigconversion_tests!(I2112, U2112, i2112);
    generate_sigconversion_tests!(I2176, U2176, i2176);
    generate_sigconversion_tests!(I3072, U3072, i3072);
    generate_sigconversion_tests!(I3136, U3136, i3136);
    generate_sigconversion_tests!(I3200, U3200, i3200);
    generate_sigconversion_tests!(I4096, U4096, i4096);
    generate_sigconversion_tests!(I4160, U4160, i4160);
    generate_sigconversion_tests!(I4224, U4224, i4224);
    generate_sigconversion_tests!(I7680, U7680, i7680);
    generate_sigconversion_tests!(I7744, U7744, i7744);
    generate_sigconversion_tests!(I7808, U7808, i7808);
    generate_sigconversion_tests!(I8192, U8192, i8192);
    generate_sigconversion_tests!(I8256, U8256, i8256);
    generate_sigconversion_tests!(I8320, U8320, i8320);
    generate_sigconversion_tests!(I15360, U15360, i15360);
    generate_sigconversion_tests!(I15424, U15424, i15424);
    generate_sigconversion_tests!(I15488, U15488, i15488);
  }
  mod sigcmp {
    use super::super::*;
    use testing::{build_test_path,run_test};

    generate_sigcmp_tests!(I256, U256, i256);
    generate_sigcmp_tests!(I320, U320, i320);
    generate_sigcmp_tests!(I576, U576, i576);
    generate_sigcmp_tests!(I1088, U1088, i1088);
    generate_sigcmp_tests!(I1600, U1600, i1600);
    generate_sigcmp_tests!(I2112, U2112, i2112);
    generate_sigcmp_tests!(I3136, U3136, i3136);
    generate_sigcmp_tests!(I4160, U4160, i4160);
    generate_sigcmp_tests!(I7744, U7744, i7744);
    generate_sigcmp_tests!(I8256, U8256, i8256);
    generate_sigcmp_tests!(I15424, U15424, i15424);
  }
  mod sigmul {
    use super::super::*;
    use testing::{build_test_path,run_test};

    generate_sigmul_tests!(I256, U256, i256, I512, U512);
    generate_sigmul_tests!(I320, U320, i320, I640, U640);
    generate_sigmul_tests!(I448, U448, i448, I896, U896);
    generate_sigmul_tests!(I640, U640, i640, I1280, U1280);
  }
  mod sigdiv {
    use super::super::*;
    use testing::{build_test_path,run_test};

    generate_sigdiv_tests!(I512, U512, i512);
    generate_sigdiv_tests!(I640, U640, i640);
    generate_sigdiv_tests!(I896, U896, i896);
    generate_sigdiv_tests!(I1280, U1280, i1280);
  }
  mod sigshiftl {
    use super::super::*;
    use testing::{build_test_path,run_test};

    generate_sigshiftl_tests!(I256, U256, i256);
    generate_sigshiftl_tests!(I320, U320, i320);
    generate_sigshiftl_tests!(I576, U576, i576);
    generate_sigshiftl_tests!(I1088, U1088, i1088);
    generate_sigshiftl_tests!(I1600, U1600, i1600);
    generate_sigshiftl_tests!(I2112, U2112, i2112);
    generate_sigshiftl_tests!(I3136, U3136, i3136);
    generate_sigshiftl_tests!(I4160, U4160, i4160);
    generate_sigshiftl_tests!(I7744, U7744, i7744);
    generate_sigshiftl_tests!(I8256, U8256, i8256);
    generate_sigshiftl_tests!(I15424, U15424, i15424);
  }
  mod sigshiftr {
    use super::super::*;
    use testing::{build_test_path,run_test};

    generate_sigshiftr_tests!(I256, U256, i256);
    generate_sigshiftr_tests!(I320, U320, i320);
    generate_sigshiftr_tests!(I576, U576, i576);
    generate_sigshiftr_tests!(I1088, U1088, i1088);
    generate_sigshiftr_tests!(I1600, U1600, i1600);
    generate_sigshiftr_tests!(I2112, U2112, i2112);
    generate_sigshiftr_tests!(I3136, U3136, i3136);
    generate_sigshiftr_tests!(I4160, U4160, i4160);
    generate_sigshiftr_tests!(I7744, U7744, i7744);
    generate_sigshiftr_tests!(I8256, U8256, i8256);
    generate_sigshiftr_tests!(I15424, U15424, i15424);
  }
  mod egcd {
    use super::super::*;
    use testing::{build_test_path,run_test};

    generate_egcd_tests!(I192, U192, i192, I256, U256);
    generate_egcd_tests!(I256, U256, i256, I320, U320);
    generate_egcd_tests!(I512, U512, i512, I576, U576);
    generate_egcd_tests!(ignore I1024, U1024, i1024, I1088, U1088);
    generate_egcd_tests!(ignore I1536, U1536, i1536, I1600, U1600);
    generate_egcd_tests!(ignore I2048, U2048, i2048, I2112, U2112);
    generate_egcd_tests!(ignore I3072, U3072, i3072, I3136, U3136);
    generate_egcd_tests!(ignore I4096, U4096, i4096, I4160, U4160);
    generate_egcd_tests!(ignore I7680, U7680, i7680, I7744, U7744);
    generate_egcd_tests!(ignore I8192, U8192, i8192, I8256, U8256);
    generate_egcd_tests!(ignore I15360, U15360, i15360, I15424, U15424);
  }
  mod modinv {
    use super::super::*;
    use testing::{build_test_path,run_test};

    generate_modinv_tests!(I192, U192, i192);
    generate_modinv_tests!(I256, U256, i256);
    generate_modinv_tests!(I512, U512, i512);
    generate_modinv_tests!(I1024, U1024, i1024);
    generate_modinv_tests!(ignore I2048, U2048, i2048);
    generate_modinv_tests!(ignore I3072, U3072, i3072);
    generate_modinv_tests!(ignore I4096, U4096, i4096);
    generate_modinv_tests!(ignore I8192, U8192, i8192);
    generate_modinv_tests!(ignore I15360, U15360, i15360);
  }
}
