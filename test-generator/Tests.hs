module Tests(
         Test
       , SizedTest
       , testDatabase
       )
 where

import           Control.Exception(assert)
import           Data.Bits(shiftL,shiftR,testBit)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import           Database
import           GHC.Integer.GMP.Internals(powModInteger,recipModInteger)
import           Math
import           Requirements

type Test      = Int -> SizedTest
type SizedTest = Database -> (Map String String, Integer, Database)

testDatabase :: [(Operation, String, String, Test)]
testDatabase = [
    (Add,         "add",            "unsigned addition",       addTest),
    (BaseOps,     "base",           "unsigned base",           baseTest),
    (BaseOps,     "cmp",            "unsigned compare",        compareTest),
    (Barretts,    "barrett_gen",    "barrett generation",      barrettGenTest),
    (Barretts,    "barrett_reduce", "barrett reduce",          barrettReduceTest),
    (Div,         "div",            "unsigned division",       divTest),
    (ModExp,      "modexp",         "modular exponentiation",  modexpTest),
    (ModMul,      "modmul",         "modular multiplication",  modmulTest),
    (ModSq,       "modsq",          "modular square",          modsqTest),
    (Mul,         "mul",            "unsigned multiplication", mulTest),
    (Shifts,      "shiftl",         "unsigned shift left",     shiftlTest),
    (Shifts,      "shiftr",         "unsigned shift right",    shiftrTest),
    (Square,      "square",         "unsigned squaring",       squareTest),
    (Sub,         "sub",            "unsigned subtraction",    subTest),
    (SignedAdd,   "sigadd",         "signed addition",         sigaddTest),
    (SignedBase,  "signed",         "signed base",             signedTest),
    (SignedCmp,   "sigcmp",         "signed compare",          sigcmpTest),
    (SignedMul,   "sigmul",         "signed multiply",         sigmulTest),
    (SignedDiv,   "sigdiv",         "signed division",         sigdivTest),
    (SignedShift, "sigshiftr",      "signed shift right",      sigshiftrTest),
    (SignedShift, "sigshiftl",      "signed shift left",       sigshiftlTest),
    (SignedSub,   "sigsub",         "signed subtraction",      sigsubTest),
    (SquareRoot,  "sqrt",           "square root",             sqrtTest),
    (EGCD,        "egcd",           "EGCD",                    egcdTest),
    (ModDiv,      "moddiv",         "modular division",        moddivTest),
    (ModInv,      "modinv",         "modular inversion",       modinvTest)
  ]

addTest :: Test
addTest size memory0 =
  let (a, memory1) = generateNum memory0 "a" size
      (b, memory2) = generateNum memory1 "b" size
      c            = a + b
      res          = Map.fromList [("a", showX a), ("b", showX b),
                                   ("c", showX c)]
  in (res, c, memory2)

barrettGenTest :: Test
barrettGenTest size memory0 =
  let (m, memory1) = generateNum memory0 "m" size
      k            = computeK m
      u            = barrett m
      res          = Map.fromList [("m", showX m), ("k", showX k),
                                   ("u", showX u)]
  in (res, u, memory1)

barrettReduceTest :: Test
barrettReduceTest size memory0 =
  let (m, memory1) = generateNum memory0 "m" size
      (x, memory2) = generateNum memory1 "x" (min (2 * size) (2 * k * 64))
      k            = computeK m
      u            = barrett m
      r            = x `mod` m
      res          = Map.fromList [("m", showX m), ("x", showX x),
                                   ("k", showX k), ("u", showX u),
                                   ("r", showX r)]
  in (res, r, memory2)

baseTest :: Test
baseTest size memory0 =
  let (x, memory1) = generateNum memory0 "x" size
      (m, memory2) = generateNum memory1 "m" size
      (b, memory3) = generateNum memory2 "b" 16
      m'           = m `mod` (fromIntegral size `div` 64)
      r            = x `mod` (2 ^ (64 * m'))
      t            = x `testBit` (fromIntegral b)
      res          = Map.fromList [("x", showX x),        ("z", showB (x == 0)),
                                   ("e", showB (even x)), ("o", showB (odd x)),
                                   ("m", showX m'),       ("r", showX r),
                                   ("b", showX b),        ("t", showB t)]
  in (res, x, memory3)

compareTest :: Test
compareTest size memory0 =
  let (a, memory1) = generateNum memory0 "a" size
      (b, memory2) = generateNum memory1 "b" size
      res          = Map.fromList [("a", showX a), ("b", showX b),
                                   ("g", showB (a > b)), ("l", showB (a < b)),
                                   ("e", showB (a == b))]
  in (res, a, memory2)

divTest :: Test
divTest size memory0 =
  let (a, memory1) = generateNum memory0 "a" size
      (b, memory2) = generateNum memory1 "b" size
      q            = a `div` b
      r            = a `mod` b
      res          = Map.fromList [("a", showX a), ("b", showX b),
                                   ("q", showX q), ("r", showX r)]
  in (res, q, memory2)

mulTest :: Test
mulTest size memory0 =
  let (a, memory1) = generateNum memory0 "a" size
      (b, memory2) = generateNum memory1 "b" size
      c            = a * b
      res          = Map.fromList [("a", showX a), ("b", showX b),
                                   ("c", showX c)]
  in (res, c, memory2)

shiftlTest :: Test
shiftlTest size memory0 =
  let (a, memory1) = generateNum memory0 "a" size
      (l, memory2) = generateNum memory1 "l" size
      l'           = l `mod` fromIntegral (computeK a * 64)
      r            = modulate (a `shiftL` fromIntegral l') size
      res          = Map.fromList [("a", showX a), ("l", showX l'), ("r", showX r)]
  in (res, r, memory2)

shiftrTest :: Test
shiftrTest size memory0 =
  let (a, memory1) = generateNum memory0 "a" size
      (l, memory2) = generateNum memory1 "l" size
      l'           = l `mod` fromIntegral (computeK a * 64)
      r            = modulate (a `shiftR` fromIntegral l') size
      res          = Map.fromList [("a", showX a), ("l", showX l'), ("r", showX r)]
  in (res, l, memory2)

subTest :: Test
subTest size memory0 =
  let (a, memory1) = generateNum memory0 "a" size
      (b, memory2) = generateNum memory1 "b" size
      c            = modulate    (a - b)     size
      res          = Map.fromList [("a", showX a), ("b", showX b),
                                   ("c", showX c)]
  in (res, c, memory2)

modsqTest :: Test
modsqTest size memory0 =
  let (a, memory1) = generateNum memory0 "a" size
      (m, memory2) = generateNum memory1 "m" size
      k            = computeK m
      u            = barrett m
      c            = (a * a) `mod` m
      res          = Map.fromList [("a", showX a), ("m", showX m),
                                   ("c", showX c), ("u", showX u),
                                   ("k", showX k)]
  in (res, c, memory2)

modmulTest :: Test
modmulTest size memory0 =
  let (a, memory1) = generateNum memory0 "a" size
      (b, memory2) = generateNum memory1 "b" size
      (m, memory3) = generateNum memory2 "m" size
      k            = computeK m
      u            = barrett m
      c            = (a * b) `mod` m
      res          = Map.fromList [("a", showX a), ("b", showX b),
                                   ("m", showX m), ("c", showX c),
                                   ("u", showX u), ("k", showX k)]
  in (res, c, memory3)

modexpTest :: Test
modexpTest size memory0 =
  let (b, memory1) = generateNum memory0 "b" size
      (e, memory2) = generateNum memory1 "e" size
      (m, memory3) = generateNum memory2 "m" size
      k            = computeK m
      u            = barrett m
      r            = powModInteger b e m
      res          = Map.fromList [("b", showX b), ("e", showX e),
                                   ("m", showX m), ("r", showX r),
                                   ("u", showX u), ("k", showX k)]
  in (res, r, memory3)

squareTest :: Test
squareTest size memory0 =
  let (a, memory1) = generateNum memory0 "a" size
      r            = modulate    (a * a)     (2 * size)
      res          = Map.fromList [("a", showX a), ("r", showX r)]
  in (res, r, memory1)

sigaddTest :: Test
sigaddTest size memory0 =
  let (a, memory1) = genSign (generateNum memory0 "a" size)
      (b, memory2) = genSign (generateNum memory1 "b" size)
      c            = a + b
      res          = Map.fromList [("a", showX a), ("b", showX b),
                                   ("c", showX c)]
  in (res, c, memory2)

sigsubTest :: Test
sigsubTest size memory0 =
  let (a, memory1) = genSign (generateNum memory0 "a" size)
      (b, memory2) = genSign (generateNum memory1 "b" size)
      c            = a - b
      res          = Map.fromList [("a", showX a), ("b", showX b),
                                   ("c", showX c)]
  in (res, c, memory2)

sqrtTest :: Test
sqrtTest size memory0 =
  let (a, memory1) = generateNum memory0 "a" size
      r            = isqrt size a
      res          = Map.fromList [("a", showX a), ("r", showX r)]
  in (res, r, memory1)

signedTest :: Test
signedTest size memory0 =
  let (x, memory1) = genSign (generateNum memory0 "x" size)
      res          = Map.fromList [("x", showX x),        ("z", showB (x == 0)),
                                   ("e", showB (even x)), ("o", showB (odd x))]
  in (res, x, memory1)

sigshiftlTest :: Test
sigshiftlTest size memory0 =
  let (a, memory1) = genSign (generateNum memory0 "a" size)
      (l, memory2) = generateNum memory1 "l" size
      l'           = l `mod` fromIntegral (computeK (abs a) * 64)
      r            = modulate' (a `shiftL` fromIntegral l') size
      res          = Map.fromList [("a", showX a), ("l", showX l'), ("r", showX r)]
  in (res, r, memory2)

sigshiftrTest :: Test
sigshiftrTest size memory0 =
  let (a, memory1) = genSign (generateNum memory0 "a" size)
      (l, memory2) = generateNum memory1 "l" size
      l'           = l `mod` fromIntegral (computeK (abs a) * 64)
      r            = modulate' (a `shiftR` fromIntegral l') size
      res          = Map.fromList [("a", showX a), ("l", showX l'), ("r", showX r)]
  in (res, l, memory2)

sigcmpTest :: Test
sigcmpTest size memory0 =
  let (a, memory1) = genSign (generateNum memory0 "a" size)
      (b, memory2) = genSign (generateNum memory1 "b" size)
      res          = Map.fromList [("a", showX a), ("b", showX b),
                                   ("g", showB (a > b)), ("l", showB (a < b)),
                                   ("e", showB (a == b))]
  in (res, a, memory2)

sigdivTest :: Test
sigdivTest size memory0 =
  let (a, memory1) = genSign (generateNum memory0 "a" size)
      (b, memory2) = genSign (generateNum memory1 "b" size)
      q            = a `div` b
      r            = a `mod` b
      res          = Map.fromList [("a", showX a), ("b", showX b),
                                   ("q", showX q), ("r", showX r)]
  in (res, q, memory2)

sigmulTest :: Test
sigmulTest size memory0 =
  let (a, memory1) = genSign (generateNum memory0 "a" size)
      (b, memory2) = genSign (generateNum memory1 "b" size)
      c            = a * b
      res          = Map.fromList [("a", showX a), ("b", showX b),
                                   ("c", showX c)]
  in (res, c, memory2)

egcdTest :: Test
egcdTest size memory0 =
  let (x, memory1) = genSign (generateNum memory0 "x" size)
      (y, memory2) = genSign (generateNum memory1 "y" size)
      (a, b, v)    = if (x >= 0) && (y >= 0)
                       then extendedGCD x y
                       else safeGCD x y
      res          = Map.fromList [("x", showX x), ("y", showX y),
                                   ("a", showX a), ("b", showX b),
                                   ("v", showX v)]
  in assert (((a * x) + (b * y)) == v) $
     assert (v == gcd x y) $
     (res, v, memory2)

moddivTest :: Test
moddivTest size memoryIn =
  let attempt memory0 =
        let (a, memory1) = genSign (generateNum memory0 "a" size)
            (b, memory2) = generateNum memory1 "b" size
            (m, memory3) = generateNum memory2 "m" size
            maybe_res    = divmod a b m
        in case maybe_res of
             Nothing ->
               attempt memory3
             Just c ->
               let res = Map.fromList [("a", showX a), ("b", showX b),
                                       ("m", showX m), ("c", showX c)]
               in (res, c, memory3)
  in attempt memoryIn

modinvTest :: Test
modinvTest size memoryIn =
  let attempt memory0 =
        let (a, memory1) = generateNum memory0 "a" size
            (b, memory2) = generateNum memory1 "b" size
            c            = recipModInteger a b
            res          = Map.fromList [("a", showX a), ("b", showX b),
                                         ("c", showX c)]
        in if c == 0
             then attempt memory2
             else assert (c < b) $
                  assert ((a * c) `mod` b == 1) $
                  (res, c, memory2)
  in attempt memoryIn

smodinvTest :: Test
smodinvTest size memoryIn =
  let attempt memory0 =
        let (a, memory1) = genSign (generateNum memory0 "a" size)
            (b, memory2) = generateNum memory1 "b" size
            c            = recipModInteger a b
            res          = Map.fromList [("a", showX a), ("b", showX b),
                                         ("c", showX c)]
        in if c == 0
             then attempt memory2
             else assert (c < b) $
                  assert ((a * c) `mod` b == 1) $
                  (res, c, memory2)
  in attempt memoryIn
