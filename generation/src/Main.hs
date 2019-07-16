module Main
 where

import Control.Monad(forM_,unless)
import Data.List(sort)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import Gen(runGen)
import Requirements(Requirement(..), Operation(..), requirements)
import System.Directory(createDirectoryIfMissing)
import System.Environment(getArgs)
import System.Exit(die)
import System.FilePath((</>))
import UnsignedBase(declareBaseStructure,declareBinaryOperators)

gatherRequirements :: [Requirement] -> Map Int [Operation]
gatherRequirements = foldr process Map.empty
 where process (Req x val) = Map.insertWith (++) x [val]

main :: IO ()
main =
  do args <- getArgs
     unless (length args == 1) $
       die ("generation takes exactly one argument, the target directory")
     let reqs = sort (Map.toList (gatherRequirements requirements))
         target = head args
     forM_ reqs $ \ (size, opes) ->
       do let basedir = target </> "unsigned" </> ("u" ++ show size)
          createDirectoryIfMissing True basedir
          forM_ reqs $ \ (x, ops) ->
            do runGen (basedir </> "mod.rs") (declareBaseStructure size ops)
               runGen (basedir </> "binary.rs") (declareBinaryOperators size)
    