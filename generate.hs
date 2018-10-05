import Control.Monad(foldM_,forM_,when)
import Data.Bits(shiftL,shiftR)
import Data.List(sort)
import qualified Data.Map.Strict as Map
import GHC.Integer.GMP.Internals(powModInteger)
import Numeric(showHex)
import Prelude hiding (log)
import System.Directory(createDirectoryIfMissing)
import System.Environment(getArgs)
import System.FilePath((</>))
import System.IO(Handle,IOMode(WriteMode),hPutStrLn,withFile,hFlush,hPutStr,stderr)
import System.Random(StdGen,newStdGen,random,split)

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
 deriving (Eq, Ord, Show)

data Requirement = Req Int Operation
 deriving (Eq, Ord, Show)

data Need = Need Operation (Int -> [Requirement])

needs :: [Need]
needs = [ Need ModExp   (\ size -> [Req size ModMul
                                   ,Req size ModSq
                                   ,Req size Barretts])
        , Need ModSq    (\ size -> [Req (size * 2) Div
                                   ,Req size Barretts
                                   ,Req size Square])
        , Need ModMul   (\ size -> [Req size Mul
                                   ,Req size Barretts
                                   ,Req size (Convert (size * 2))
                                   ,Req (size * 2) Div])
        , Need Barretts (\ size -> [Req (size + 64) BaseOps
                                   ,Req size (Convert (size + 64))
                                   ,Req (size + 64) (Convert ((size * 2) + 64))
                                   ,Req size (Convert ((size * 2) + 64))
                                   ,Req ((size * 2) + 64) Add
                                   ,Req ((size * 2) + 64) Sub
                                   ,Req (size + 64) Mul
                                   ,Req (size * 2) (Convert ((size * 2) + 64))
                                   ,Req ((size * 2) + 64) Shifts
                                   ,Req ((size * 2) + 128) Shifts
                                   ,Req ((size * 2) + 64) Div
                                   ,Req (size + 64) (Convert (size * 2))
                                   ,Req (size + 64) (Convert ((size * 2) + 128))
                                   ,Req ((size * 2) + 64)
                                        (Convert ((size * 2) + 128))
                                   ])
        , Need Div      (\ size -> [Req size (Convert (size * 2))
                                   ,Req 192 BaseOps
                                   ,Req 384 BaseOps
                                   ,Req 192 Mul
                                   ,Req size Mul
                                   ,Req size Shifts
                                   ,Req (size * 2) Sub
                                   ])
        , Need Mul      (\ size -> [Req (size * 2) BaseOps])
        , Need Sub      (\ size -> [Req size Add])
        , Need Add      (\ size -> [Req (size + 64) BaseOps
                                   ,Req size (Convert (size + 64))])
        ]

newRequirements :: Requirement -> [Requirement]
newRequirements (Req size op) = concatMap go needs ++ [Req size BaseOps]
 where
  go (Need op2 generator) | op == op2 = generator size
                          | otherwise = []

bitSizes :: [Int]
bitSizes =  [192,256,384,512,576,1024,2048,3072,4096,7680,8192,15360]

baseRequirements :: [Requirement]
baseRequirements = map (\ x -> Req x ModExp) bitSizes

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
                     String -> Operation -> Bool -> [Int -> Int] ->
                     IO ()
generateTestBlock hndl name level useRT addOns =
  do hPutStrLn hndl ("  mod " ++ name ++ " {")
     when useRT $
       do hPutStrLn hndl ("    use super::super::*;")
          hPutStrLn hndl ("    use testing::run_test;")
          hPutStrLn hndl ""
     forM_ requirements $ \ (Req size kind) ->
       when (kind == level) $
         hPutStrLn hndl ("    generate_" ++ name ++ 
                         "_tests!(U" ++ show size ++ ", " ++
                         "u" ++ show size ++
                         concatMap (\ f -> ", U" ++ show (f size)) addOns ++
                         ");")
     hPutStrLn hndl "  }"

generateInvocs :: IO ()
generateInvocs =
  withFile "src/unsigned/invoc.rs" WriteMode $ \ hndl ->
    do forM_ requirements $ \ (Req size oper) ->
         case oper of
           Add        -> hPutStrLn hndl ("addition_impls!(U" ++ show size ++ ", U" ++ show (size + 64) ++ ");")
           BaseOps    -> hPutStrLn hndl ("base_impls!(U" ++ show size ++ ", " ++ show (size `div` 64) ++ ");")
           Barretts   -> hPutStrLn hndl ("barrett_impl!(BarrettU" ++ show size ++ ", U" ++ show size ++ ", U" ++ show (size + 64) ++ ", U" ++ show (size * 2) ++ ", U" ++ show ((size * 2) + 64) ++ ");")
           Div        -> hPutStrLn hndl ("div_impls!(U" ++ show size ++ ", U" ++ show (size * 2) ++ ");")
           ModExp     -> hPutStrLn hndl ("modexp_impls!(U" ++ show size ++ ");")
           ModMul     -> hPutStrLn hndl ("modmul_impls!(U" ++ show size ++ ", U" ++ show (size * 2) ++ ");")
           ModSq      -> hPutStrLn hndl ("modsq_impls!(U" ++ show size ++ ", U" ++ show (size * 2) ++ ");")
           Mul        -> hPutStrLn hndl ("multiply_impls!(U" ++ show size ++ ", U" ++ show (size * 2) ++ ");")
           Shifts     -> hPutStrLn hndl ("shift_impls!(U" ++ show size ++ ", " ++ show (size `div` 64) ++ ");")
           Square     -> hPutStrLn hndl ("square_impls!(U" ++ show size ++ ", U" ++ show (size * 2) ++ ", " ++ show size ++ ");")
           Sub        -> hPutStrLn hndl ("subtraction_impls!(U" ++ show size ++ ", " ++ show (size `div` 64) ++ ");")
           Convert to -> hPutStrLn hndl ("conversion_impls!(U" ++ show size ++ ", U" ++ show to ++ ");")
       hPutStrLn hndl ""
       hPutStrLn hndl "\n#[cfg(test)]"
       hPutStrLn hndl "mod tests {"
       generateTestBlock hndl "base"        BaseOps  True  []
       generateTestBlock hndl "conversion"  BaseOps  False []
       generateTestBlock hndl "codec"       BaseOps  False []
       generateTestBlock hndl "cmp"         BaseOps  True  []
       generateTestBlock hndl "sub"         Sub      True  []
       generateTestBlock hndl "shiftl"      Shifts   True  []
       generateTestBlock hndl "shiftr"      Shifts   True  []
       generateTestBlock hndl "add"         Add      True  [(+ 64)]
       generateTestBlock hndl "mul"         Mul      True  [(* 2)]
       generateTestBlock hndl "div"         Div      True  []
       generateTestBlock hndl "barrett_gen" Barretts True  [(+ 64)]
       generateTestBlock hndl "barrett_red" Barretts True  [(+ 64), (* 2)]
       generateTestBlock hndl "modsq"       ModSq    True  []
       generateTestBlock hndl "modmul"      ModMul   True  []
       generateTestBlock hndl "modexp"      ModExp   True  []
       generateTestBlock hndl "square"      Square   True  [(* 2)]
       hPutStrLn hndl "}"

log :: String -> IO ()
log str = hPutStr stderr str >> hFlush stderr

generateTests :: Operation -> String -> Database ->
                 (Int -> Database -> (Map.Map String String, Integer, Database)) ->
                 IO ()
generateTests op directory init runner = do
  forM_ (getSizes op requirements) $ \ size ->
    do createDirectoryIfMissing True ("testdata" </> directory)
       log $ "Generating " ++ show size ++ "-bit " ++ directory ++ " tests: 000%"
       let dest = "testdata" </> directory </> ("U" ++ show size ++ ".tests") 
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
    do let (output, key, acc@(db',_)) = runner size db
           before = Map.findWithDefault [] "RESULT" db'
       if length (filter (== key) before) >= 10
          then writer hndl size runner acc x
          else do forM_ (Map.toList output) $ \ (key, val) ->
                    do hPutStrLn hndl (key ++ ": " ++ val)
                       let val = (x * 100) `div` numberOfTests
                       log ("\b\b\b\b" ++ pad 3 ' ' (show val) ++ "%")
                  return acc
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

modulate :: Integer -> Int -> Integer
modulate x size = x `mod` (2 ^ size)

showX :: (Integral a, Show a) => a -> String
showX x = showHex x ""

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
     generateTests Add "add" db1 $ \ size memory0 ->
       let (a, memory1) = generateNum memory0 "a" size
           (b, memory2) = generateNum memory1 "b" size
           c            = a + b
           res          = Map.fromList [("a", showX a), ("b", showX b),
                                        ("c", showX c)]
       in (res, c, memory2)
     let (db2, gen2) = emptyDatabase gen1
     generateTests Barretts "barrett_gen" db2 $ \ size memory0 ->
       let (m, memory1) = generateNum memory0 "m" size
           k            = computeK m
           u            = barrett m
           res          = Map.fromList [("m", showX m), ("k", showX k),
                                        ("u", showX u)]
       in (res, u, memory1)
     let (db3, gen3) = emptyDatabase gen2
     generateTests Barretts "barrett_reduce" db3 $ \ size memory0 ->
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
     generateTests BaseOps "base" db4 $ \ size memory0 ->
       let (x, memory1) = generateNum memory0 "x" size
           (m, memory2) = generateNum memory1 "m" size
           m'           = m `mod` (fromIntegral size `div` 64)
           r            = x `mod` (2 ^ (64 * m'))
           res          = Map.fromList [("x", showX x),        ("z", showB (x == 0)),
                                        ("e", showB (even x)), ("o", showB (odd x)),
                                        ("m", showX m'),       ("r", showX r)]
       in (res, x, memory2)
     let (db5, gen5) = emptyDatabase gen4
     generateTests BaseOps "cmp" db5 $ \ size memory0 ->
       let (a, memory1) = generateNum memory0 "a" size
           (b, memory2) = generateNum memory1 "b" size
           res          = Map.fromList [("a", showX a), ("b", showX b),
                                        ("g", showB (a > b)), ("l", showB (a < b)),
                                        ("e", showB (a == b))]
       in (res, a, memory2)
     let (db6, gen6) = emptyDatabase gen5
     generateTests Div "div" db6 $ \ size memory0 ->
       let (a, memory1) = generateNum memory0 "a" size
           (b, memory2) = generateNum memory1 "b" size
           q            = a `div` b
           r            = a `mod` b
           res          = Map.fromList [("a", showX a), ("b", showX b),
                                        ("q", showX q), ("r", showX r)]
       in (res, q, memory2)
     let (db7, gen7) = emptyDatabase gen6
     generateTests Mul "mul" db7 $ \ size memory0 ->
       let (a, memory1) = generateNum memory0 "a" size
           (b, memory2) = generateNum memory1 "b" size
           c            = a * b
           res          = Map.fromList [("a", showX a), ("b", showX b),
                                        ("c", showX c)]
       in (res, c, memory2)
     let (db8, gen8) = emptyDatabase gen7
     generateTests Shifts "shiftl" db8 $ \ size memory0 ->
       let (a, memory1) = generateNum memory0 "a" size
           (l, memory2) = generateNum memory1 "l" size
           l'           = l `mod` fromIntegral (computeK a * 64)
           r            = modulate (a `shiftL` fromIntegral l') size
           res          = Map.fromList [("a", showX a), ("l", showX l'), ("r", showX r)]
       in (res, r, memory2)
     let (db9, gen9) = emptyDatabase gen8
     generateTests Shifts "shiftr" db9 $ \ size memory0 ->
       let (a, memory1) = generateNum memory0 "a" size
           (l, memory2) = generateNum memory1 "l" size
           l'           = l `mod` fromIntegral (computeK a * 64)
           r            = modulate (a `shiftR` fromIntegral l') size
           res          = Map.fromList [("a", showX a), ("l", showX l'), ("r", showX r)]
       in (res, l, memory2)
     let (dbA, genA) = emptyDatabase gen9
     generateTests Sub "sub" dbA $ \ size memory0 ->
       let (a, memory1) = generateNum memory0 "a" size
           (b, memory2) = generateNum memory1 "b" size
           c            = modulate    (a - b)     size
           res          = Map.fromList [("a", showX a), ("b", showX b),
                                        ("c", showX c)]
       in (res, c, memory2)
     let (dbB, genB) = emptyDatabase genA
     generateTests ModSq "modsq" dbB $ \ size memory0 ->
       let (a, memory1) = generateNum memory0 "a" size
           (m, memory2) = generateNum memory1 "m" size
           c            = (a * a) `mod` m
           res          = Map.fromList [("a", showX a), ("m", showX m),
                                        ("c", showX c)]
       in (res, c, memory2)
     let (dbC, genC) = emptyDatabase genB
     generateTests ModMul "modmul" dbC $ \ size memory0 ->
       let (a, memory1) = generateNum memory0 "a" size
           (b, memory2) = generateNum memory1 "b" size
           (m, memory3) = generateNum memory2 "m" size
           c            = (a * b) `mod` m
           res          = Map.fromList [("a", showX a), ("b", showX b),
                                        ("m", showX m), ("c", showX c)]
       in (res, c, memory3)
     let (dbD, genD) = emptyDatabase genC
     generateTests ModExp "modexp" dbD $ \ size memory0 ->
       let (b, memory1) = generateNum memory0 "b" size
           (e, memory2) = generateNum memory1 "e" size
           (m, memory3) = generateNum memory2 "m" size
           r            = powModInteger b e m
           res          = Map.fromList [("b", showX b), ("e", showX e),
                                        ("m", showX m), ("r", showX r)]
       in (res, r, memory3)
     let (dbE, genE) = emptyDatabase genC
     generateTests Square "square" dbE $ \ size memory0 ->
       let (a, memory1) = generateNum memory0 "a" size
           r            = modulate    (a * a)     (2 * size)
           res          = Map.fromList [("a", showX a), ("r", showX r)]
       in (res, r, memory1)

main :: IO ()
main =
  do args <- getArgs
     let args' = if null args then ["invocs", "tests"] else args
     when ("invocs" `elem` args') generateInvocs
     when ("tests"  `elem` args') generateAllTheTests
