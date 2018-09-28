import Control.Monad(foldM_,forM_,when)
import Data.Bits(shiftL,shiftR)
import Data.List(sort)
import qualified Data.Map.Strict as Map
import Numeric(showHex)
import Prelude hiding (log)
import System.Directory(createDirectoryIfMissing)
import System.Environment(getArgs)
import System.FilePath((</>))
import System.IO(Handle,IOMode(WriteMode),hPutStrLn,withFile,hFlush,hPutStr,stderr)
import System.Random(StdGen,newStdGen,random,split)

data Level = Base | DivMul | Barrett
 deriving (Eq, Ord, Show)

bitSizes :: [Int]
bitSizes =  [192,256,384,512,576,1024,2048,3072,4096,7680,8192,15360]

numberOfTests :: Int
numberOfTests = 1000

baseMap :: Map.Map Int Level
baseMap = foldr (\ s m -> Map.insert s Barrett m) Map.empty bitSizes

smartInsert :: Int -> Level -> Map.Map Int Level -> Map.Map Int Level
smartInsert size level map = Map.insertWith max size level map

generateNext :: Int -> Level -> Map.Map Int Level -> Map.Map Int Level
generateNext size Base    acc = acc
generateNext size DivMul  acc = smartInsert (size + 64) Base $
                                smartInsert (size * 2)  Base $
                                acc
generateNext size Barrett acc = smartInsert (size + 64)       Base   $
                                smartInsert (size * 2)        Base   $
                                smartInsert ( size      + 64) DivMul $
                                smartInsert ((size * 2) + 64) DivMul $
                                acc

step :: Map.Map Int Level -> Map.Map Int Level
step m = Map.foldrWithKey generateNext m m

fixpoint :: Map.Map Int Level -> Map.Map Int Level
fixpoint m | m == m'   = m
           | otherwise = fixpoint m'
 where m' = step m

finalMap :: Map.Map Int Level
finalMap = fixpoint baseMap

conversions :: [Int] -> [(Int,Int)]
conversions []       = []
conversions (x:rest) = (map (\ y -> (x,y)) rest) ++ conversions rest

generateTestBlock :: Handle -> String -> Level -> Bool -> [Int -> Int] -> IO ()
generateTestBlock hndl name level useRT addOns =
  do hPutStrLn hndl ("  mod " ++ name ++ " {")
     when useRT $
       do hPutStrLn hndl ("    use super::super::*;")
          hPutStrLn hndl ("    use testing::run_test;")
          hPutStrLn hndl ""
     forM_ (sort (Map.toList finalMap)) $ \ (size, kind) ->
       when (kind >= level) $
         hPutStrLn hndl ("    generate_" ++ name ++ 
                         "_tests!(U" ++ show size ++ ", " ++
                         "u" ++ show size ++
                         concatMap (\ f -> ", U" ++ show (f size)) addOns ++
                         ");")
     hPutStrLn hndl "  }"

generateInvocs :: IO ()
generateInvocs =
  withFile "src/unsigned/invoc.rs" WriteMode $ \ hndl ->
    do forM_ (sort (Map.toList finalMap)) $ \ item ->
         case item of
           (size, Base) ->
             hPutStrLn hndl ("generate_number!(U" ++ show size ++ ", " ++
                             show (size `div` 64) ++ ");")
           (size, DivMul) ->
             hPutStrLn hndl ("generate_number!(U" ++ show size ++ ", " ++
                             show (size `div` 64) ++ ", U" ++
                             show (size + 64) ++ ", U" ++
                             show (size * 2) ++ ");")
           (size, Barrett) ->
             hPutStrLn hndl ("generate_number!(U" ++ show size ++ ", " ++
                             show (size `div` 64) ++ ", U" ++
                             show (size + 64) ++ ", U" ++
                             show (size * 2) ++ ", U" ++
                             show ((size * 2) + 64) ++ ", BarrettU" ++
                             show size ++ ");")
       hPutStrLn hndl ""
       forM_ (conversions (Map.keys finalMap)) $ \ (a,b) ->
         hPutStrLn hndl ("conversion_impls!(U" ++ show a ++ ", " ++
                         "U" ++ show b ++ ");")
       hPutStrLn hndl "\n#[cfg(test)]"
       hPutStrLn hndl "mod tests {"
       generateTestBlock hndl "base"       Base   True  []
       generateTestBlock hndl "conversion" Base   False []
       generateTestBlock hndl "codec"      Base   False []
       generateTestBlock hndl "cmp"        Base   True  []
       generateTestBlock hndl "sub"        Base   True  []
       generateTestBlock hndl "shiftl"     Base   True  []
       generateTestBlock hndl "shiftr"     Base   True  []
       generateTestBlock hndl "add"        DivMul True  [(+ 64)]
       generateTestBlock hndl "mul"        DivMul True  [(* 2)]
       generateTestBlock hndl "div"        DivMul True  []
       hPutStrLn hndl "}"

log :: String -> IO ()
log str = hPutStr stderr str >> hFlush stderr

generateTests :: Level -> String -> a -> (Int -> a -> (Map.Map String String, a)) -> IO ()
generateTests minLevel directory init runner =
  forM_  (sort (Map.toList finalMap)) $ \ (size, myLevel) ->
    when (myLevel >= minLevel) $
      do createDirectoryIfMissing True ("testdata" </> directory)
         log $ "Generating " ++ show size ++ "-bit " ++ directory ++ " tests "
         let dest = "testdata" </> directory </> ("U" ++ show size ++ ".tests") 
         withFile dest WriteMode $ \ hndl ->
           foldM_ (writer hndl size runner) init [0..numberOfTests]
         log "done.\n"
 where
  writer :: Handle -> Int -> (Int -> a -> (Map.Map String String, a)) -> a -> Int -> IO a
  writer hndl size runner input _ =
    do let (output, acc) = runner size input
       forM_ (Map.toList output) $ \ (key, val) ->
         do hPutStrLn hndl (key ++ ": " ++ val)
            log "."
       return acc

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
     generateTests DivMul "add" db1 $ \ size memory0 ->
       let (a, memory1) = generateNum memory0 "a" size
           (b, memory2) = generateNum memory1 "b" size
           c            = a + b
       in (Map.fromList [("a", showX a), ("b", showX b),("c", showX c)], memory2)
     let (db2, gen2) = emptyDatabase gen1
     generateTests Barrett "barrett_gen" db2 $ \ size memory0 ->
       let (m, memory1) = generateNum memory0 "m" size
           k            = computeK m
           u            = barrett m
       in (Map.fromList [("m", showX m), ("k", showX k), ("u", showX u)], memory1)
     let (db3, gen3) = emptyDatabase gen1
     generateTests Barrett "barrett_reduce" db3 $ \ size memory0 ->
       let (m, memory1) = generateNum memory0 "m" size
           (x, memory2) = generateNum memory1 "x" (min size (2 * k * 64))
           k            = computeK m
           u            = barrett m
           r            = x `mod` m
           res          = Map.fromList [("m", showX m), ("x", showX x),
                                        ("k", showX k), ("u", showX u),
                                        ("r", showX r)]
       in (res, memory2)
     let (db4, gen4) = emptyDatabase gen2
     generateTests Base "base" db4 $ \ size memory0 ->
       let (x, memory1) = generateNum memory0 "x" size
           (m, memory2) = generateNum memory1 "m" size
           m'           = m `mod` (fromIntegral size `div` 64)
           r            = x `mod` (2 ^ (64 * m'))
           res          = Map.fromList [("x", showX x),        ("z", showB (x == 0)),
                                        ("e", showB (even x)), ("o", showB (odd x)),
                                        ("m", showX m'),       ("r", showX r)]
       in (res, memory2)
     let (db5, gen5) = emptyDatabase gen3
     generateTests Base "cmp" db5 $ \ size memory0 ->
       let (a, memory1) = generateNum memory0 "a" size
           (b, memory2) = generateNum memory1 "b" size
           res          = Map.fromList [("a", showX a), ("b", showX b),
                                        ("g", showB (a > b)), ("l", showB (a < b)),
                                        ("e", showB (a == b))]
       in (res, memory2)
     let (db6, gen6) = emptyDatabase gen4
     generateTests DivMul "div" db6 $ \ size memory0 ->
       let (a, memory1) = generateNum memory0 "a" size
           (b, memory2) = generateNum memory1 "b" size
           q            = a `div` b
           r            = a `mod` b
           res          = Map.fromList [("a", showX a), ("b", showX b),
                                        ("q", showX q), ("r", showX r)]
       in (res, memory2)
     let (db7, gen7) = emptyDatabase gen5
     generateTests DivMul "mul" db7 $ \ size memory0 ->
       let (a, memory1) = generateNum memory0 "a" size
           (b, memory2) = generateNum memory1 "b" size
           c            = a * b
           res          = Map.fromList [("a", showX a), ("b", showX b),
                                        ("c", showX c)]
       in (res, memory2)
     let (db8, gen8) = emptyDatabase gen6
     generateTests Base "shiftl" db8 $ \ size memory0 ->
       let (a, memory1) = generateNum memory0 "a" size
           (l, memory2) = generateNum memory1 "l" size
           l'           = l `mod` fromIntegral (computeK a * 64)
           r            = modulate (a `shiftL` fromIntegral l') size
           res          = Map.fromList [("a", showX a), ("l", showX l'), ("r", showX r)]
       in (res, memory2)
     let (db9, gen9) = emptyDatabase gen6
     generateTests Base "shiftr" db9 $ \ size memory0 ->
       let (a, memory1) = generateNum memory0 "a" size
           (l, memory2) = generateNum memory1 "l" size
           l'           = l `mod` fromIntegral (computeK a * 64)
           r            = modulate (a `shiftR` fromIntegral l') size
           res          = Map.fromList [("a", showX a), ("l", showX l'), ("r", showX r)]
       in (res, memory2)
     let (dbA, genA) = emptyDatabase gen7
     generateTests Base "sub" dbA $ \ size memory0 ->
       let (a, memory1) = generateNum memory0 "a" size
           (b, memory2) = generateNum memory1 "b" size
           c            = modulate    (a - b)     size
       in (Map.fromList [("a", showX a), ("b", showX b), ("c", showX c)], memory2)

main :: IO ()
main =
  do args <- getArgs
     let args' = if null args then ["invocs", "test"] else args
     when ("invocs" `elem` args') generateInvocs
     when ("tests"  `elem` args') generateAllTheTests
