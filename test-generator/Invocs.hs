import Control.Monad
import Requirements
import System.IO

generateTestBlock :: Handle ->
                     String -> Operation -> Bool -> Int -> [Int -> Int] ->
                     IO ()
generateTestBlock hndl name level useRT ignoreAt addOns =
  do hPutStrLn hndl ("  mod " ++ name ++ " {")
     when useRT $
       do hPutStrLn hndl ("    use super::super::*;")
          hPutStrLn hndl ("    use testing::{build_test_path,run_test};")
          hPutStrLn hndl ""
     forM_ requirements $ \ (Req size kind) ->
       when (kind == level) $
         hPutStrLn hndl ("    generate_" ++ name ++ 
                         "_tests!(" ++
                         (if size >= ignoreAt then "ignore " else "") ++
                         "U" ++ show size ++ ", " ++
                         "u" ++ show size ++
                         concatMap (\ f -> ", U" ++ show (f size)) addOns ++
                         ");")
     hPutStrLn hndl "  }"

generateSigTestBlock :: Handle ->
                        String -> Operation -> Bool -> Int ->
                        [Int -> Int] -> [Int -> Int] ->
                        IO ()
generateSigTestBlock hndl name level useRT ignoreAt addOns uaddOns =
  do hPutStrLn hndl ("  mod " ++ name ++ " {")
     when useRT $
       do hPutStrLn hndl ("    use super::super::*;")
          hPutStrLn hndl ("    use testing::{build_test_path,run_test};")
          hPutStrLn hndl ""
     forM_ requirements $ \ (Req size kind) ->
       when (kind == level) $
         hPutStrLn hndl ("    generate_" ++ name ++ 
                         "_tests!(" ++
                         (if size >= ignoreAt then "ignore " else "") ++
                         "I" ++ show size ++ ", " ++
                         "U" ++ show size ++ ", " ++
                         "i" ++ show size ++
                         concatMap (\ f -> ", I" ++ show (f size)) addOns ++
                         concatMap (\ f -> ", U" ++ show (f size)) uaddOns ++
                         ");")
     hPutStrLn hndl "  }"

main :: IO ()
main = do
  withFile "../src/unsigned/invoc.rs" WriteMode $ \ hndl ->
    do forM_ requirements $ \ (Req size oper) ->
         case oper of
           Add        -> hPutStrLn hndl ("addition_impls!(U" ++ show size ++ ", U" ++ show (size + 64) ++ ");")
           BaseOps    -> hPutStrLn hndl ("base_impls!(U" ++ show size ++ ", " ++ show (size `div` 64) ++ ");") >>
                         hPutStrLn hndl ("random_impls!(U" ++ show size ++ ", UniformU" ++ show size ++ ");")
           Barretts   -> hPutStrLn hndl ("barrett_impl!(BarrettU" ++ show size ++ ", U" ++ show size ++ ", U" ++ show (size + 64) ++ ", U" ++ show (size * 2) ++ ", U" ++ show ((size * 2) + 64) ++ ");")
           Div        -> hPutStrLn hndl ("div_impls!(U" ++ show size ++ ", U" ++ show (size * 2) ++ ");")
           ModExp     -> hPutStrLn hndl ("modexp_impls!(U" ++ show size ++ ", U" ++ show size ++ ");") >>
                         hPutStrLn hndl ("modexp_impls!(U" ++ show size ++ ", BarrettU" ++ show size ++ ");")
           ModMul     -> hPutStrLn hndl ("modmul_impls!(U" ++ show size ++ ", U" ++ show (size * 2) ++ ", BarrettU" ++ show size ++ ");")
           ModSq      -> hPutStrLn hndl ("modsq_impls!(U" ++ show size ++ ", U" ++ show (size * 2) ++ ", BarrettU" ++ show size ++ ");")
           Mul        -> hPutStrLn hndl ("multiply_impls!(U" ++ show size ++ ", U" ++ show (size * 2) ++ ");")
           Shifts     -> hPutStrLn hndl ("shift_impls!(U" ++ show size ++ ", " ++ show (size `div` 64) ++ ");")
           Square     -> hPutStrLn hndl ("square_impls!(U" ++ show size ++ ", U" ++ show (size * 2) ++ ", " ++ show size ++ ");")
           Sub        -> hPutStrLn hndl ("subtraction_impls!(U" ++ show size ++ ", " ++ show (size `div` 64) ++ ");")
           Convert to -> hPutStrLn hndl ("conversion_impls!(U" ++ show size ++ ", U" ++ show to ++ ");")
           PrimeGen   -> hPutStrLn hndl ("prime_gen_impls!(U" ++ show size ++ ");")
           _          -> return ()
       hPutStrLn hndl ""
       hPutStrLn hndl "\n#[cfg(test)]"
       hPutStrLn hndl "mod tests {"
       generateTestBlock hndl "base"           BaseOps  True  16384 []
       generateTestBlock hndl "conversion"     BaseOps  False 90000 []
       generateTestBlock hndl "codec"          BaseOps  False 90000 []
       generateTestBlock hndl "cmp"            BaseOps  True  16384 []
       generateTestBlock hndl "sub"            Sub      True  9000  []
       generateTestBlock hndl "shiftl"         Shifts   True  9000  []
       generateTestBlock hndl "shiftr"         Shifts   True  9000  []
       generateTestBlock hndl "add"            Add      True  9000  [(+ 64)]
       generateTestBlock hndl "mul"            Mul      True  9000  [(* 2)]
       generateTestBlock hndl "div"            Div      True  2049  []
       generateTestBlock hndl "barrett_gen"    Barretts True  2000  [(+ 64)]
       generateTestBlock hndl "barrett_red"    Barretts True  4000  [(+ 64), (* 2)]
       generateTestBlock hndl "modsq"          ModSq    True  4000  []
       generateTestBlock hndl "modmul"         ModMul   True  4000  []
       generateTestBlock hndl "modexp"         ModExp   True  512   []
       generateTestBlock hndl "square"         Square   True  4000  [(* 2)]
       generateTestBlock hndl "barrett_modsq"  ModSq    True  4000  [(+ 64)]
       generateTestBlock hndl "barrett_modmul" ModMul   True  4000  [(+ 64)]
       generateTestBlock hndl "barrett_modexp" ModExp   True  1024  [(+ 64)]
       hPutStrLn hndl "}"
  withFile "../src/signed/invoc.rs" WriteMode $ \ hndl ->
    do forM_ requirements $ \ (Req size oper) ->
         case oper of
           SignedAdd    -> hPutStrLn hndl ("add_impls!(I" ++ show size ++ ", I" ++ show (size + 64) ++ ", U" ++ show (size + 64) ++ ");")
           SignedBase   -> hPutStrLn hndl ("signed_impls!(I" ++ show size ++ ", U" ++ show size ++ ");")
           SignedCmp    -> hPutStrLn hndl ("cmp_impls!(I" ++ show size ++ ");")
           SignedShift  -> hPutStrLn hndl ("shift_impls!(I" ++ show size ++ ", U" ++ show size ++ ");")
           SignedSub    -> hPutStrLn hndl ("subtraction_impls!(I" ++ show size ++ ", I" ++ show (size + 64) ++ ", U" ++ show (size + 64) ++ ");")
           EGCD         -> hPutStrLn hndl ("egcd_impls!(I" ++ show (size + 64) ++ ", U" ++ show size ++ ", I" ++ show size ++ ");")
           ModInv       -> hPutStrLn hndl ("modinv_impls!(U" ++ show size ++ ", I" ++ show (size + 64) ++ ", U" ++ show (size + 64) ++ ");")
           SigConvert v -> hPutStrLn hndl ("conversion_impls!(I" ++ show size ++ ", U" ++ show size ++ ", I" ++ show v ++ ", U" ++ show v ++ ");")
           _            -> return ()
       hPutStrLn hndl ""
       hPutStrLn hndl "\n#[cfg(test)]"
       hPutStrLn hndl "mod tests {"
       generateSigTestBlock hndl "sigadd"        SignedAdd   True  16384 [(+ 64)] [(+ 64)]
       generateSigTestBlock hndl "sigsub"        SignedSub   True  16384 [(+ 64)] [(+ 64)]
       generateSigTestBlock hndl "signed"        SignedBase  True  90000 []       []
       generateSigTestBlock hndl "sigconversion" SignedBase  False 90000 []       []
       generateSigTestBlock hndl "sigcmp"        SignedCmp   True  90000 []       []
       generateSigTestBlock hndl "sigshiftl"     SignedShift True  16384 []       []
       generateSigTestBlock hndl "sigshiftr"     SignedShift True  16384 []       []
       generateSigTestBlock hndl "egcd"          EGCD        True  1024  [(+ 64)] [(+ 64)]
       generateSigTestBlock hndl "modinv"        ModInv      True  2048  []       []
       hPutStrLn hndl "}"
