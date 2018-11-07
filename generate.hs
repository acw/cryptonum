{-# LANGUAGE RecordWildCards #-}
import Control.Exception(assert)
import Control.Monad(foldM_,forM_,when)
import Data.Bits(shiftL,shiftR)
import Data.List(sort)
import qualified Data.Map.Strict as Map
import GHC.Integer.GMP.Internals(powModInteger,recipModInteger)
import Numeric(showHex)
import Prelude hiding (log)
import System.Directory(createDirectoryIfMissing)
import System.Environment(getArgs)
import System.FilePath((</>))
import System.IO(Handle,IOMode(WriteMode),hPutStrLn,withFile,hFlush,hPutStr,stderr)
import System.Random(StdGen,newStdGen,random,split)

import Debug.Trace

data Operation = Add
               | BaseOps
               | Barretts
               | Div
               | ModExp
               | ModMul
               | ModSq
               | Mul
               | Shifts
               | Square
               | Sub
               | Convert Int
               | SignedAdd
               | SignedBase
               | SignedCmp
               | SignedShift
               | SignedSub
               | SigConvert Int
               | EGCD
               | ModInv
               | RSA
 deriving (Eq, Ord, Show)

data Requirement = Req Int Operation
 deriving (Eq, Ord, Show)

data Need = Need Operation (Int -> [Requirement])

needs :: [Need]
needs = [ Need RSA         (\ size -> [Req (size `div` 2) Sub,
                                       Req (size `div` 2) Mul,
                                       Req size           BaseOps,
                                       Req size           ModInv,
                                       Req size           ModExp
                                      ])
        , Need Add         (\ size -> [Req size BaseOps,
                                       Req (size + 64) BaseOps,
                                       Req size (Convert (size + 64))
                                      ])
        , Need Barretts    (\ size -> [Req size BaseOps,
                                       Req (size + 64) BaseOps,
                                       Req (size * 2) BaseOps,
                                       Req ((size * 2) + 64) BaseOps,
                                       Req size (Convert ((size * 2) + 64)),
                                       Req (size + 64) Mul,
                                       Req ((size * 2) + 64) Add,
                                       Req ((size * 2) + 64) Sub,
                                       Req (size + 64) (Convert ((size * 2) + 64)),
                                       Req ((size * 2) + 64) (Convert ((size + 64) * 2)),
                                       Req (size * 2) (Convert ((size * 2) + 64)),
                                       Req (size + 64) (Convert ((size + 64) * 2)),
                                       Req (size + 64) (Convert (size * 2)),
                                       Req (size * 2) Shifts,
                                       Req ((size + 64) * 2) Shifts,
                                       Req ((size * 2) + 64) Div
                                      ])
        , Need Div         (\ size -> [Req size BaseOps,
                                       Req (size * 2) BaseOps,
                                       Req size (Convert (size * 2)),
                                       Req (size * 2) Sub,
                                       Req size Mul,
                                       Req 192 BaseOps,
                                       Req 192 Mul,
                                       Req 384 BaseOps
                                      ])
        , Need ModExp      (\ size -> [Req size BaseOps,
                                       Req size Barretts,
                                       Req size ModSq,
                                       Req size ModMul
                                      ])
        , Need ModMul      (\ size -> [Req size BaseOps,
                                       Req (size * 2) BaseOps,
                                       Req size Barretts,
                                       Req size Mul
                                      ])
        , Need ModSq       (\ size -> [Req size BaseOps,
                                       Req (size * 2) BaseOps,
                                       Req size Barretts,
                                       Req size Square,
                                       Req (size * 2) Div,
                                       Req size (Convert (size * 2))
                                      ])
        , Need Mul         (\ size -> [Req size BaseOps,
                                       Req (size * 2) BaseOps,
                                       Req size (Convert (size * 2))
                                      ])
        , Need Shifts      (\ size -> [Req size BaseOps
                                      ])
        , Need Square      (\ size -> [Req size BaseOps,
                                       Req (size * 2) BaseOps
                                      ])
        , Need Sub         (\ size -> [Req size BaseOps
                                      ])
        , Need SignedAdd   (\ size -> [Req size SignedBase,
                                       Req (size + 64) SignedBase,
                                       Req (size + 64) BaseOps
                                      ])
        , Need SignedBase  (\ size -> [Req size BaseOps])
        , Need SignedCmp   (\ size -> [Req size BaseOps])
        , Need SignedShift (\ size -> [Req size SignedBase,
                                       Req size BaseOps,
                                       Req size Shifts,
                                       Req size Add
                                      ])
        , Need SignedSub   (\ size -> [Req size SignedBase,
                                       Req (size + 64) SignedBase,
                                       Req (size + 64) BaseOps,
                                       Req size Add,
                                       Req size Sub,
                                       Req size (Convert (size + 64)),
                                       Req size (SigConvert (size + 64))
                                      ])
        , Need EGCD        (\ size -> [Req size SignedBase,
                                       Req size BaseOps,
                                       Req (size + 64) SignedBase,
                                       Req size (SigConvert (size + 64)),
                                       Req (size + 64) SignedShift,
                                       Req (size + 64) SignedAdd,
                                       Req (size + 64) SignedSub,
                                       Req (size + 64) SignedCmp
                                      ])
        , Need ModInv      (\ size -> [Req size BaseOps,
                                       Req (size + 64) SignedBase,
                                       Req (size + 64) BaseOps,
                                       Req size (Convert (size + 64)),
                                       Req size EGCD,
                                       Req (size + 64) SignedAdd,
                                       Req size Barretts
                                      ])
        ]
-- needs = [ Need ModExp      (\ size -> [Req size ModMul
--                                       ,Req size ModSq
--                                       ,Req size Barretts])
--         , Need ModSq       (\ size -> [Req (size * 2) Div
--                                       ,Req size Barretts
--                                       ,Req size Square])
--         , Need ModMul      (\ size -> [Req size Mul
--                                       ,Req size Barretts
--                                       ,Req size (Convert (size * 2))
--                                       ,Req (size * 2) Div])
--         , Need Barretts    (\ size -> [Req (size + 64) BaseOps
--                                       ,Req size (Convert (size + 64))
--                                       ,Req (size + 64) (Convert ((size * 2) + 64))
--                                       ,Req size (Convert ((size * 2) + 64))
--                                       ,Req ((size * 2) + 64) Add
--                                       ,Req ((size * 2) + 64) Sub
--                                       ,Req (size + 64) Mul
--                                       ,Req (size * 2) (Convert ((size * 2) + 64))
--                                       ,Req ((size * 2) + 64) Shifts
--                                       ,Req ((size * 2) + 128) Shifts
--                                       ,Req ((size * 2) + 64) Div
--                                       ,Req (size + 64) (Convert (size * 2))
--                                       ,Req (size + 64) (Convert ((size * 2) + 128))
--                                       ,Req ((size * 2) + 64)
--                                            (Convert ((size * 2) + 128))
--                                       ])
--         , Need Div         (\ size -> [Req size (Convert (size * 2))
--                                       ,Req 192 BaseOps
--                                       ,Req 384 BaseOps
--                                       ,Req 192 Mul
--                                       ,Req size Mul
--                                       ,Req size Shifts
--                                       ,Req (size * 2) Sub
--                                       ])
--         , Need Mul         (\ size -> [Req (size * 2) BaseOps])
--         , Need Sub         (\ size -> [Req size Add])
--         , Need Add         (\ size -> [Req (size + 64) BaseOps
--                                       ,Req size (Convert (size + 64))])
--         , Need ModInv      (\ size -> [Req size SignedBase,
--                                        Req size EGCD])
--         , Need EGCD        (\ size -> [Req size BaseOps,
--                                        Req (size + 64) SignedBase,
--                                        Req size (SigConvert (size + 64)),
--                                        Req (size + 64) SignedShift,
--                                        Req (size + 64) SignedAdd,
--                                        Req (size + 64) SignedSub,
--                                        Req (size + 64) SignedCmp
--                                        ])
--         , Need SignedShift (\ size -> [Req size Shifts, Req size Add])
--         , Need SignedAdd   (\ size -> [Req size Sub,
--                                        Req (size + 64) Add,
--                                        Req (size + 64) SignedBase,
--                                        Req size (SigConvert (size + 64))
--                                        ])
--         , Need SignedSub   (\ size -> [Req (size + 64) SignedBase,
--                                        Req size (SigConvert (size + 64)),
--                                        Req size Sub
--                                        ])
--         , Need RSA         (\ size -> [Req size ModExp, Req size ModInv,
--                                        Req (size `div` 2) Sub,
--                                        Req (size `div` 2) Mul])
--         ]

newRequirements :: Requirement -> [Requirement]
newRequirements (Req size op) = concatMap go needs ++ [Req size BaseOps]
 where
  go (Need op2 generator) | op == op2 = generator size
                          | otherwise = []

rsaSizes :: [Int]
rsaSizes =  [512,1024,2048,3072,4096,8192,15360]

baseRequirements :: [Requirement]
baseRequirements = concatMap (\ x -> [Req x RSA]) rsaSizes
                ++ [Req 192 Add, Req 256 Add, Req 384 Add] -- used for testing
                ++ [Req 192 Mul, Req 384 Mul] -- used for testing
                ++ [Req 448 (Convert 512)] -- used for testing

requirements :: [Requirement]
requirements = go baseRequirements
 where
  step ls = let news = concatMap newRequirements ls
                destBits = concatMap destRequirements (news ++ ls)
            in ls ++ news ++ destBits
  --
  go ls = let ls' = removeDups (sort (step ls))
          in if ls == ls' then ls else go ls'
  --
  removeDups [] = []
  removeDups (x:xs) | x `elem` xs = removeDups xs
                    | otherwise   = x : removeDups xs
  --
  destRequirements (Req _ (Convert t)) = [Req t BaseOps]
  destRequirements _                   = []

numberOfTests :: Int
numberOfTests = 1000

generateTestBlock :: Handle ->
                     String -> Operation -> Bool -> Int -> [Int -> Int] ->
                     IO ()
generateTestBlock hndl name level useRT ignoreAt addOns =
  do hPutStrLn hndl ("  mod " ++ name ++ " {")
     when useRT $
       do hPutStrLn hndl ("    use super::super::*;")
          hPutStrLn hndl ("    use testing::run_test;")
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
          hPutStrLn hndl ("    use testing::run_test;")
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

generateInvocs :: IO ()
generateInvocs = do
  withFile "src/unsigned/invoc.rs" WriteMode $ \ hndl ->
    do forM_ requirements $ \ (Req size oper) ->
         case oper of
           Add        -> hPutStrLn hndl ("addition_impls!(U" ++ show size ++ ", U" ++ show (size + 64) ++ ");")
           BaseOps    -> hPutStrLn hndl ("base_impls!(U" ++ show size ++ ", " ++ show (size `div` 64) ++ ");")
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
  withFile "src/signed/invoc.rs" WriteMode $ \ hndl ->
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

log :: String -> IO ()
log str = hPutStr stderr str >> hFlush stderr

generateTests :: String -> Operation -> String -> Database ->
                 (Int -> Database -> (Map.Map String String, Integer, Database)) ->
                 IO ()
generateTests prefix op directory init runner = do
  forM_ (getSizes op requirements) $ \ size ->
    do createDirectoryIfMissing True ("testdata" </> directory)
       log $ "Generating " ++ show size ++ "-bit " ++ directory ++ " tests: 000%"
       let dest = "testdata" </> directory </> (prefix ++ show size ++ ".tests") 
       withFile dest WriteMode $ \ hndl ->
         foldM_ (writer hndl size runner) init [0..numberOfTests]
       log "\n"
 where
  getSizes :: Operation -> [Requirement] -> [Int]
  getSizes _    [] = []
  getSizes oper ((Req size oper2) : rest)
    | oper == oper2 = size : getSizes oper rest
    | otherwise     = getSizes oper rest
  --
  writer hndl size runner db x =
    do let (output, key, acc@(db',gen')) = runner size db
           before = Map.findWithDefault [] "RESULT" db'
       if length (filter (== key) before) >= 10
          then writer hndl size runner acc x
          else do forM_ (Map.toList output) $ \ (key, val) ->
                    do hPutStrLn hndl (key ++ ": " ++ val)
                       let val = (x * 100) `div` numberOfTests
                       log ("\b\b\b\b" ++ pad 3 ' ' (show val) ++ "%")
                  return (Map.insert "RESULT" (key : before) db', gen')
  --
  pad x c str | length str < x = pad x c (c : str)
              | otherwise      = str

type Database = (Map.Map String [Integer], StdGen)

emptyDatabase :: StdGen -> (Database, StdGen)
emptyDatabase g0 =
  let (g, g') = split g0
  in ((Map.empty, g), g')

generateNum :: Database -> String -> Int -> (Integer, Database)
generateNum (db, rng0) varname size =
  let (x, rng1) = random rng0
      x'        = modulate x size
      before    = Map.findWithDefault [] varname db
  in if length (filter (== x') before) < 10
       then (x', (Map.insert varname (x':before) db, rng1))
       else generateNum (db, rng1) varname size

genSign :: (Integer, Database) -> (Integer, Database)
genSign (x, (db, rng0)) =
  let (n, rng1) = random rng0
  in if n then (0 - x, (db, rng1)) else (x, (db, rng1))

modulate :: Integer -> Int -> Integer
modulate x size = x `mod` (2 ^ size)

modulate' :: Integer -> Int -> Integer
modulate' x size = signum x * (abs x `mod` (2 ^ size))

showX :: (Integral a, Show a) => a -> String
showX x | x < 0     = "-" ++ showX (abs x)
        | otherwise = showHex x ""

showB :: Bool -> String
showB False = "0"
showB True  = "1"

base :: Integer
base = 2 ^ (64 :: Integer)

barrett :: Integer -> Integer
barrett m = (base ^ (2 * k)) `div` m
 where
  k = computeK m

computeK :: Integer -> Int
computeK v = go 0 1
 where
  go k acc | v <= acc  = k
           | otherwise = go (k + 1) (acc * base)

generateAllTheTests :: IO ()
generateAllTheTests =
  do gen0 <- newStdGen
     let (db1, gen1) = emptyDatabase gen0
     generateTests "U" Add "add" db1 $ \ size memory0 ->
       let (a, memory1) = generateNum memory0 "a" size
           (b, memory2) = generateNum memory1 "b" size
           c            = a + b
           res          = Map.fromList [("a", showX a), ("b", showX b),
                                        ("c", showX c)]
       in (res, c, memory2)
     let (db2, gen2) = emptyDatabase gen1
     generateTests "U" Barretts "barrett_gen" db2 $ \ size memory0 ->
       let (m, memory1) = generateNum memory0 "m" size
           k            = computeK m
           u            = barrett m
           res          = Map.fromList [("m", showX m), ("k", showX k),
                                        ("u", showX u)]
       in (res, u, memory1)
     let (db3, gen3) = emptyDatabase gen2
     generateTests "U" Barretts "barrett_reduce" db3 $ \ size memory0 ->
       let (m, memory1) = generateNum memory0 "m" size
           (x, memory2) = generateNum memory1 "x" (min (2 * size) (2 * k * 64))
           k            = computeK m
           u            = barrett m
           r            = x `mod` m
           res          = Map.fromList [("m", showX m), ("x", showX x),
                                        ("k", showX k), ("u", showX u),
                                        ("r", showX r)]
       in (res, r, memory2)
     let (db4, gen4) = emptyDatabase gen3
     generateTests "U" BaseOps "base" db4 $ \ size memory0 ->
       let (x, memory1) = generateNum memory0 "x" size
           (m, memory2) = generateNum memory1 "m" size
           m'           = m `mod` (fromIntegral size `div` 64)
           r            = x `mod` (2 ^ (64 * m'))
           res          = Map.fromList [("x", showX x),        ("z", showB (x == 0)),
                                        ("e", showB (even x)), ("o", showB (odd x)),
                                        ("m", showX m'),       ("r", showX r)]
       in (res, x, memory2)
     let (db5, gen5) = emptyDatabase gen4
     generateTests "U" BaseOps "cmp" db5 $ \ size memory0 ->
       let (a, memory1) = generateNum memory0 "a" size
           (b, memory2) = generateNum memory1 "b" size
           res          = Map.fromList [("a", showX a), ("b", showX b),
                                        ("g", showB (a > b)), ("l", showB (a < b)),
                                        ("e", showB (a == b))]
       in (res, a, memory2)
     let (db6, gen6) = emptyDatabase gen5
     generateTests "U" Div "div" db6 $ \ size memory0 ->
       let (a, memory1) = generateNum memory0 "a" size
           (b, memory2) = generateNum memory1 "b" size
           q            = a `div` b
           r            = a `mod` b
           res          = Map.fromList [("a", showX a), ("b", showX b),
                                        ("q", showX q), ("r", showX r)]
       in (res, q, memory2)
     let (db7, gen7) = emptyDatabase gen6
     generateTests "U" Mul "mul" db7 $ \ size memory0 ->
       let (a, memory1) = generateNum memory0 "a" size
           (b, memory2) = generateNum memory1 "b" size
           c            = a * b
           res          = Map.fromList [("a", showX a), ("b", showX b),
                                        ("c", showX c)]
       in (res, c, memory2)
     let (db8, gen8) = emptyDatabase gen7
     generateTests "U" Shifts "shiftl" db8 $ \ size memory0 ->
       let (a, memory1) = generateNum memory0 "a" size
           (l, memory2) = generateNum memory1 "l" size
           l'           = l `mod` fromIntegral (computeK a * 64)
           r            = modulate (a `shiftL` fromIntegral l') size
           res          = Map.fromList [("a", showX a), ("l", showX l'), ("r", showX r)]
       in (res, r, memory2)
     let (db9, gen9) = emptyDatabase gen8
     generateTests "U" Shifts "shiftr" db9 $ \ size memory0 ->
       let (a, memory1) = generateNum memory0 "a" size
           (l, memory2) = generateNum memory1 "l" size
           l'           = l `mod` fromIntegral (computeK a * 64)
           r            = modulate (a `shiftR` fromIntegral l') size
           res          = Map.fromList [("a", showX a), ("l", showX l'), ("r", showX r)]
       in (res, l, memory2)
     let (dbA, genA) = emptyDatabase gen9
     generateTests "U" Sub "sub" dbA $ \ size memory0 ->
       let (a, memory1) = generateNum memory0 "a" size
           (b, memory2) = generateNum memory1 "b" size
           c            = modulate    (a - b)     size
           res          = Map.fromList [("a", showX a), ("b", showX b),
                                        ("c", showX c)]
       in (res, c, memory2)
     let (dbB, genB) = emptyDatabase genA
     generateTests "U" ModSq "modsq" dbB $ \ size memory0 ->
       let (a, memory1) = generateNum memory0 "a" size
           (m, memory2) = generateNum memory1 "m" size
           k            = computeK m
           u            = barrett m
           c            = (a * a) `mod` m
           res          = Map.fromList [("a", showX a), ("m", showX m),
                                        ("c", showX c), ("u", showX u),
                                        ("k", showX k)]
       in (res, c, memory2)
     let (dbC, genC) = emptyDatabase genB
     generateTests "U" ModMul "modmul" dbC $ \ size memory0 ->
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
     let (dbD, genD) = emptyDatabase genC
     generateTests "U" ModExp "modexp" dbD $ \ size memory0 ->
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
     let (dbE, genE) = emptyDatabase genD
     generateTests "U" Square "square" dbE $ \ size memory0 ->
       let (a, memory1) = generateNum memory0 "a" size
           r            = modulate    (a * a)     (2 * size)
           res          = Map.fromList [("a", showX a), ("r", showX r)]
       in (res, r, memory1)
     --
     let (dbF, genF) = emptyDatabase genE
     generateTests "I" SignedAdd "sigadd" dbF $ \ size memory0 ->
       let (a, memory1) = genSign (generateNum memory0 "a" size)
           (b, memory2) = genSign (generateNum memory1 "b" size)
           c            = a + b
           res          = Map.fromList [("a", showX a), ("b", showX b),
                                        ("c", showX c)]
       in (res, c, memory2)
     let (dbG, genG) = emptyDatabase genF
     generateTests "I" SignedSub "sigsub" dbG $ \ size memory0 ->
       let (a, memory1) = genSign (generateNum memory0 "a" size)
           (b, memory2) = genSign (generateNum memory1 "b" size)
           c            = a - b
           res          = Map.fromList [("a", showX a), ("b", showX b),
                                        ("c", showX c)]
       in (res, c, memory2)
     let (dbH, genH) = emptyDatabase genG
     generateTests "I" SignedBase "signed" dbH $ \ size memory0 ->
       let (x, memory1) = genSign (generateNum memory0 "x" size)
           res          = Map.fromList [("x", showX x),        ("z", showB (x == 0)),
                                        ("e", showB (even x)), ("o", showB (odd x))]
       in (res, x, memory1)
     let (dbI, genI) = emptyDatabase genH
     generateTests "I" SignedShift "sigshiftl" dbI $ \ size memory0 ->
       let (a, memory1) = genSign (generateNum memory0 "a" size)
           (l, memory2) = generateNum memory1 "l" size
           l'           = l `mod` fromIntegral (computeK (abs a) * 64)
           r            = modulate' (a `shiftL` fromIntegral l') size
           res          = Map.fromList [("a", showX a), ("l", showX l'), ("r", showX r)]
       in (res, r, memory2)
     let (dbJ, genJ) = emptyDatabase genI
     generateTests "I" SignedShift "sigshiftr" dbJ $ \ size memory0 ->
       let (a, memory1) = genSign (generateNum memory0 "a" size)
           (l, memory2) = generateNum memory1 "l" size
           l'           = l `mod` fromIntegral (computeK (abs a) * 64)
           r            = modulate' (a `shiftR` fromIntegral l') size
           res          = Map.fromList [("a", showX a), ("l", showX l'), ("r", showX r)]
       in (res, l, memory2)
     let (dbK, genK) = emptyDatabase genJ
     generateTests "I" SignedCmp "sigcmp" dbK $ \ size memory0 ->
       let (a, memory1) = genSign (generateNum memory0 "a" size)
           (b, memory2) = genSign (generateNum memory1 "b" size)
           res          = Map.fromList [("a", showX a), ("b", showX b),
                                        ("g", showB (a > b)), ("l", showB (a < b)),
                                        ("e", showB (a == b))]
       in (res, a, memory2)
     let (dbL, genL) = emptyDatabase genK
     generateTests "I" EGCD "egcd" dbL $ \ size memory0 ->
       let (x, memory1)  = generateNum memory0 "x" size
           (y, memory2)  = generateNum memory1 "y" size
           ans@(a, b, v) = extendedGCD x y
           res           = Map.fromList [("x", showX x), ("y", showX y),
                                         ("a", showX a), ("b", showX b),
                                         ("v", showX v)]
       in assert (v == gcd x y) (res, v, memory2)
     let (dbM, genM) = emptyDatabase genL
     generateTests "I" ModInv "modinv" dbM $ \ size memoryIn ->
       let attempt memory0 =
             let (a, memory1) = generateNum memory0 "a" size
                 (b, memory2) = generateNum memory1 "b" size
                 c            = recipModInteger a b
                 res          = Map.fromList [("a", showX a), ("b", showX b),
                                              ("c", showX c)]
             in if c == 0 then attempt memory2 else (res, c, memory2)
       in attempt memoryIn

data AlgState = AlgState {
    u    :: Integer,
    v    :: Integer,
    bigA :: Integer,
    bigB :: Integer,
    bigC :: Integer,
    bigD :: Integer
}

printState :: AlgState -> IO ()
printState a =
  do putStrLn ("u: " ++ showX (u a))
     putStrLn ("v: " ++ showX (v a))
     putStrLn ("A: " ++ showX (bigA a))
     putStrLn ("B: " ++ showX (bigB a))
     putStrLn ("C: " ++ showX (bigC a))
     putStrLn ("D: " ++ showX (bigD a))

extendedGCD :: Integer -> Integer -> (Integer, Integer, Integer)
extendedGCD x y = (a, b, g * (v finalState))
  where
    (x', y', g, initState) = initialState x y 1
    finalState             = runAlgorithm x' y' initState
    a                      = bigC finalState
    b                      = bigD finalState

initialState :: Integer -> Integer -> Integer -> (Integer, Integer, Integer, AlgState)
initialState x y g | even x && even y = initialState (x `div` 2) (y `div` 2) (g * 2)
                   | otherwise        = (x, y, g, AlgState x y 1 0 0 1)

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
step6 input@AlgState{..}
  | u >= v    = AlgState (u - v) v (bigA - bigC) (bigB - bigD) bigC bigD
  | otherwise = AlgState u (v - u) bigA bigB (bigC - bigA) (bigD - bigB)

main :: IO ()
main =
  do args <- getArgs
     let args' = if null args then ["invocs", "tests"] else args
     when ("invocs" `elem` args') generateInvocs
     when ("tests"  `elem` args') generateAllTheTests

---

run :: Integer -> Integer -> IO ()
run inputx inputy =
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
