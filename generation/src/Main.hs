module Main
 where

import Base(base)
import BinaryOps(binaryOps)
import Compare(comparisons)
import Conversions(conversions)
import CryptoNum(cryptoNum)
import Control.Monad(forM_,unless)
import Data.Maybe(mapMaybe)
import File(File,Task(..),addModuleTasks,makeTask)
import Gen(runGen)
import System.Directory(createDirectoryIfMissing)
import System.Environment(getArgs)
import System.Exit(die)
import System.FilePath(takeDirectory,(</>))

lowestBitsize :: Word
lowestBitsize = 192

highestBitsize :: Word
highestBitsize = 512

bitsizes :: [Word]
bitsizes = [lowestBitsize,lowestBitsize+64..highestBitsize]

unsignedFiles :: [File]
unsignedFiles = [
    base
  , binaryOps
  , comparisons
  , conversions
  , cryptoNum
  ]

signedFiles :: [File]
signedFiles = [
  ]

makeTasks :: FilePath -> [File] -> [Task]
makeTasks basePath files =
  concatMap (\ sz -> mapMaybe (makeTask basePath sz bitsizes) files) bitsizes

makeAllTasks :: FilePath -> [Task]
makeAllTasks basePath = addModuleTasks basePath $
  makeTasks (basePath </> "unsigned") unsignedFiles ++
  makeTasks (basePath </> "signed")   signedFiles

main :: IO ()
main =
  do args <- getArgs
     unless (length args == 1) $
       die ("generation takes exactly one argument, the target directory")
     let tasks = makeAllTasks (head args)
         total = length tasks
     forM_ (zip [(1::Word)..] tasks) $ \ (i, task) ->
       do putStrLn ("[" ++ show i ++ "/" ++ show total ++ "] " ++ outputFile task)
          createDirectoryIfMissing True (takeDirectory (outputFile task))
          runGen (outputFile task) (fileGenerator task)
