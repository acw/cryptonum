module Database(
         Database,
         emptyDatabase,
         generateNum, genSign
       )
 where

import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import           Math(modulate)
import           System.Random(StdGen, random)

type Database = (Map String [Integer], StdGen)

emptyDatabase :: StdGen -> Database
emptyDatabase g0 = (Map.empty, g0)

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

