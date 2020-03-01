{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
module ModInv(
    generateModInvOps
  )
 where

import Control.Exception(assert)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import Generators
import GHC.Integer.GMP.Internals(recipModInteger)
import Language.Rust.Data.Ident
import Language.Rust.Data.Position
import Language.Rust.Quote
import Language.Rust.Syntax
import RustModule
import System.Random(RandomGen)

numTestCases :: Int
numTestCases = 100

generateModInvOps :: RustModule
generateModInvOps = RustModule {
    predicate = \ me others -> (me + 64) `elem` others,
    outputName = "modinv",
    isUnsigned = True,
    generator = declareModInv,
    testCase = Just generateModInvTests
}

declareModInv :: Word -> [Word] -> SourceFile Span 
declareModInv bitsize _ =
    let sname = mkIdent ("I" ++ show (bitsize + 64))
        uname = mkIdent ("U" ++ show bitsize)
        testFileLit = Lit [] (Str (testFile True bitsize) Cooked Unsuffixed mempty) mempty
    in [sourceFile|
        use core::convert::TryFrom;
        use crate::{CryptoNum,ModularInversion};
        use crate::signed::$$sname;
        #[cfg(test)]
        use crate::testing::{build_test_path,run_test};
        use crate::unsigned::$$uname;

        impl ModularInversion for $$uname {
            type Signed = $$sname;

            fn modinv(&self, phi: &$$uname) -> Option<$$uname>
            {
                let (_, mut b, g) = phi.egcd(&self);

                if g != $$sname::from(1i64) {
                    return None;
                }

                let sphi = $$sname::from(phi);

                while b.is_negative() {
                    b += &sphi;
                }

                if b > sphi {
                    b -= &sphi;
                }

                Some($$uname::try_from(b).expect("overflow/underflow in modinv result"))
            }

            fn egcd(&self, rhs: &$$uname) -> ($$sname, $$sname, $$sname) {
                // INPUT: two positive integers x and y.
                let mut x = $$sname::from(self);
                let mut y = $$sname::from(rhs);
                // OUTPUT: integers a, b, and v such that ax + by = v,
                //         where v = gcd(x, y).
                // 1. g←1.
                let mut gshift: usize = 0;
                // 2. While x and y are both even, do the following: x←x/2,
                //    y←y/2, g←2g.
                while x.is_even() && y.is_even() {
                    x >>= 1u64;
                    y >>= 1u64;
                    gshift += 1;
                }
                // 3. u←x, v←y, A←1, B←0, C←0, D←1.
                let mut u = x.clone();
                let mut v = y.clone();
                #[allow(non_snake_case)]
                let mut A = $$sname::from(1i64);
                #[allow(non_snake_case)]
                let mut B = $$sname::zero();
                #[allow(non_snake_case)]
                let mut C = $$sname::zero();
                #[allow(non_snake_case)]
                let mut D = $$sname::from(1i64);
                loop {
                    // 4. While u is even do the following:
                    while u.is_even() {
                        // 4.1 u←u/2.
                        u >>= 1u64;
                        // 4.2 If A≡B≡0 (mod 2) then A←A/2, B←B/2; otherwise,
                        //     A←(A + y)/2, B←(B − x)/2.
                        if A.is_even() && B.is_even() {
                            A >>= 1u64;
                            B >>= 1u64;
                        } else {
                            A += &y;
                            A >>= 1u64;
                            B -= &x;
                            B >>= 1u64;
                        }
                    }
                    // 5. While v is even do the following:
                    while v.is_even() {
                        // 5.1 v←v/2.
                        v >>= 1u64;
                        // 5.2 If C ≡ D ≡ 0 (mod 2) then C←C/2, D←D/2; otherwise,
                        //     C←(C + y)/2, D←(D − x)/2.
                        if C.is_even() && D.is_even() {
                            C >>= 1u64;
                            D >>= 1u64;
                        } else {
                            C += &y;
                            C >>= 1u64;
                            D -= &x;
                            D >>= 1u64;
                        }
                    }
                    // 6. If u≥v then u←u−v, A←A−C,B←B−D;
                    //       otherwise,v←v−u, C←C−A, D←D−B.
                    if u >= v {
                        u -= &v;
                        A -= &C;
                        B -= &D;
                    } else {
                        v -= &u;
                        C -= &A;
                        D -= &B;
                    }
                    // 7. If u = 0, then a←C, b←D, and return(a, b, g · v);
                    //        otherwise, go to step 4.
                    if u.is_zero() {
                        return (C, D, v << gshift);
                    }
                }
            }

            fn gcd_is_one(&self, b: &$$uname) -> bool {
                let mut u = self.clone();
                let mut v = b.clone();
                let one = $$uname::from(1u64);

                if u.is_zero() {
                    return v == one;
                }

                if v.is_zero() {
                    return u == one;
                }

                if u.is_even() && v.is_even() {
                    return false;
                }

                while u.is_even() {
                    u >>= 1u64;
                }

                loop {
                    while v.is_even() {
                        v >>= 1u64;
                    }
                    // u and v guaranteed to be odd right now.
                    if u > v {
                        // make sure that v > u, so that our subtraction works
                        // out.
                        let t = u;
                        u = v;
                        v = t;
                    }
                    v -= &u;

                    if v.is_zero() {
                        return u == one;
                    }
                }
            }
        }

        #[cfg(test)]
        #[allow(non_snake_case)]
        #[test]
        fn KATs() {
          run_test(build_test_path("modinv", $$(testFileLit)), 6, |case| {
            let (neg0, xbytes) = case.get("x").unwrap();
            let (neg1, ybytes) = case.get("y").unwrap();
            let (neg2, zbytes) = case.get("z").unwrap();
            let (neg3, abytes) = case.get("a").unwrap();
            let (neg4, bbytes) = case.get("b").unwrap();
            let (neg5, vbytes) = case.get("v").unwrap();
 
            assert!(!neg0 && !neg1 && !neg2);
            let     x = $$uname::from_bytes(xbytes);
            let     y = $$uname::from_bytes(ybytes);
            let     z = $$uname::from_bytes(zbytes);
            let mut a = $$sname::from_bytes(abytes);
            let mut b = $$sname::from_bytes(bbytes);
            let mut v = $$sname::from_bytes(vbytes);
 
            if *neg3 { a = -a }
            if *neg4 { b = -b }
            if *neg5 { v = -v }

            let (mya, myb, myv) = x.egcd(&y);
            assert_eq!(a, mya);
            assert_eq!(b, myb);
            assert_eq!(v, myv);

            assert_eq!(z, x.modinv(&y).expect("Didn't find a modinv?"));
            assert_eq!(v == $$sname::from(1u64), x.gcd_is_one(&y));
          });
        }
     |]

generateModInvTests :: RandomGen g => Word -> g -> [Map String String]
generateModInvTests size g = go g numTestCases
 where
  go _  0 = []
  go g0 i =
   let (x, g1)   = generateNum g0 size
       (y, g2)   = generateNum g1 size
       z         = recipModInteger x y
       (a, b, v) = extendedGCD x y
       tcase     = Map.fromList [("x", showX x), ("y", showX y),
                                 ("z", showX z), ("a", showX a),
                                 ("b", showX b), ("v", showX v)]
   in if z == 0
        then go g2 i
        else assert (z < y) $
             assert ((x * z) `mod` y == 1) $
             assert (((a * x) + (b * y)) == v) $
             assert (v == gcd x y) $
             tcase : go g2 (i - 1)

extendedGCD :: Integer -> Integer -> (Integer, Integer, Integer)
extendedGCD x y = (a, b, g * (v finalState))
  where
    (x', y', g, initState) = initialState x y 1
    finalState             = runAlgorithm x' y' initState
    a                      = bigC finalState
    b                      = bigD finalState

data AlgState = AlgState {
    u    :: Integer,
    v    :: Integer,
    bigA :: Integer,
    bigB :: Integer,
    bigC :: Integer,
    bigD :: Integer
}

initialState :: Integer -> Integer -> Integer -> (Integer, Integer, Integer, AlgState)
initialState x y g | even x && even y = initialState (x `div` 2) (y `div` 2) (g * 2)
                   | otherwise        = (x, y, g, AlgState x y 1 0 0 1)

printState :: AlgState -> IO ()
printState a =
  do putStrLn ("u: " ++ showX (u a))
     putStrLn ("v: " ++ showX (v a))
     putStrLn ("A: " ++ showX (bigA a))
     putStrLn ("B: " ++ showX (bigB a))
     putStrLn ("C: " ++ showX (bigC a))
     putStrLn ("D: " ++ showX (bigD a))

runAlgorithm :: Integer -> Integer -> AlgState -> AlgState
runAlgorithm x y state | u state == 0 = state
                       | otherwise    = runAlgorithm x y state6
  where
    state4 = step4 x y state
    state5 = step5 x y state4
    state6 = step6     state5

step4 :: Integer -> Integer -> AlgState -> AlgState
step4 x y input@AlgState{..} | even u    = step4 x y input'
                             | otherwise = input
  where
    input' = AlgState u' v bigA' bigB' bigC bigD
    u'     = u `div` 2
    bigA' | even bigA && even bigB = bigA `div` 2
          | otherwise              = (bigA + y) `div` 2
    bigB' | even bigA && even bigB = bigB `div` 2
          | otherwise              = (bigB - x) `div` 2

step5 :: Integer -> Integer -> AlgState -> AlgState
step5 x y input@AlgState{..} | even v    = step5 x y input'
                             | otherwise = input
  where
    input' = AlgState u v' bigA bigB bigC' bigD'
    v'     = v `div` 2
    bigC' | even bigC && even bigD = bigC `div` 2
          | otherwise              = (bigC + y) `div` 2
    bigD' | even bigC && even bigD = bigD `div` 2
          | otherwise              = (bigD - x) `div` 2

step6 :: AlgState -> AlgState
step6 AlgState{..}
  | u >= v    = AlgState (u - v) v (bigA - bigC) (bigB - bigD) bigC bigD
  | otherwise = AlgState u (v - u) bigA bigB (bigC - bigA) (bigD - bigB)

_run :: Integer -> Integer -> IO ()
_run inputx inputy =
  do let (x, y, g, initState) = initialState inputx inputy 1
     finalState <- go x y initState
     putStrLn ("-- FINAL STATE -----------------------")
     printState finalState
     putStrLn ("Final value: " ++ showX (g * v finalState))
     putStrLn ("-- RUN ------")
     printState (runAlgorithm x y initState)
     putStrLn ("-- NORMAL ------")
     let (a, b, v) = extendedGCD inputx inputy
     putStrLn ("a: " ++ showX a)
     putStrLn ("b: " ++ showX b)
     putStrLn ("v: " ++ showX v)

 where
  go x y state =
    do putStrLn "-- STATE -----------------------------"
       printState state
       if u state == 0
          then return state
          else do let state'   = step4 x y state
                      state''  = step5 x y state'
                      state''' = step6     state''
                  go x y state'''
