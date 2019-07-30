module Conversions(
    conversions
  )
 where

import Data.List(intercalate)
import File
import Gen

conversions :: File
conversions = File {
  predicate = \ _ _ -> True,
  outputName = "conversions",
  generator = declareConversions
}

declareConversions :: Word -> Gen ()
declareConversions bitsize =
  do let name = "U" ++ show bitsize
         entries = bitsize `div` 64
     out "use core::convert::{From,TryFrom};"
     out "#[cfg(test)]"
     out "use quickcheck::quickcheck;"
     out ("use super::" ++ name ++ ";")
     blank
     buildUnsignedPrimConversions name entries "u8"    >> blank
     buildUnsignedPrimConversions name entries "u16"   >> blank
     buildUnsignedPrimConversions name entries "u32"   >> blank
     buildUnsignedPrimConversions name entries "u64"   >> blank
     buildUnsignedPrimConversions name entries "usize" >> blank
     buildSignedPrimConversions   name entries "i8"    >> blank
     buildSignedPrimConversions   name entries "i16"   >> blank
     buildSignedPrimConversions   name entries "i32"   >> blank
     buildSignedPrimConversions   name entries "i64"   >> blank
     buildSignedPrimConversions   name entries "isize"
     blank
     out ("#[cfg(test)]")
     wrapIndent "quickcheck!" $
       do roundTripTest name "u8"     >> blank
          roundTripTest name "u16"    >> blank
          roundTripTest name "u32"    >> blank
          roundTripTest name "u64"    >> blank
          roundTripTest name "usize"

buildUnsignedPrimConversions :: String -> Word -> String -> Gen ()
buildUnsignedPrimConversions name entries primtype =
  do implFor ("From<" ++ primtype ++ ">") name $
       wrapIndent ("fn from(x: " ++ primtype ++ ") -> Self") $
         do let zeroes = replicate (fromIntegral (entries - 1)) "0,"
                values =  ("x as u64," : zeroes)
            out (name ++ " { value: [ ")
            indent $ printBy 8 values
            out ("] }")
     blank
     implFor ("From<" ++ name ++ ">") primtype $
       wrapIndent ("fn from(x: " ++ name ++ ") -> Self") $
         out ("x.value[0] as " ++ primtype)
     blank
     implFor' ("From<&'a " ++ name ++ ">") primtype $
       wrapIndent ("fn from(x: &" ++ name ++ ") -> Self") $
         out ("x.value[0] as " ++ primtype)

buildSignedPrimConversions :: String -> Word -> String -> Gen ()
buildSignedPrimConversions name entries primtype =
  do implFor ("TryFrom<" ++ primtype ++ ">") name $
       do out ("type Error = &'static str;")
          blank
          wrapIndent ("fn try_from(x: " ++ primtype ++ ") -> Result<Self,Self::Error>") $
            do wrapIndent ("if x < 0") $
                 out ("return Err(\"Attempt to convert negative number to " ++
                      name ++ ".\");")
               blank
               let zeroes = replicate (fromIntegral (entries - 1)) "0,"
                   values =  ("x as u64," : zeroes)
               out ("Ok(" ++ name ++ " { value: [ ")
               indent $ printBy 8 values
               out ("] })")
     blank
     implFor ("From<" ++ name ++ ">") primtype $
       wrapIndent ("fn from(x: " ++ name ++ ") -> Self") $
         out ("x.value[0] as " ++ primtype)
     blank
     implFor' ("From<&'a " ++ name ++ ">") primtype $
       wrapIndent ("fn from(x: &" ++ name ++ ") -> Self") $
         out ("x.value[0] as " ++ primtype)

roundTripTest :: String -> String -> Gen ()
roundTripTest name primtype =
  wrapIndent ("fn " ++ primtype ++ "_roundtrips(x: " ++ primtype ++ ") -> bool") $
    do out ("let big = " ++ name ++ "::from(x);");
       out ("let small = " ++ primtype ++ "::from(big);")
       out ("x == small")

printBy :: Int -> [String] -> Gen ()
printBy amt xs
  | length xs <= amt = out (intercalate " " xs)
  | otherwise        = printBy amt (take amt xs) >>
                       printBy amt (drop amt xs)