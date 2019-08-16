module Main
 where

import Base(base)
import BinaryOps(binaryOps)
import Compare(comparisons)
import Conversions(conversions)
import CryptoNum(cryptoNum)
import Control.Monad(forM_,unless)
import Data.Word(Word)
import File(File,Task(..),addModuleTasks,makeTasks)
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

makeTasks' :: FilePath -> FilePath -> [File] -> [Task]
makeTasks' srcPath testPath files =
  concatMap (\ sz -> concatMap (makeTasks srcPath testPath sz bitsizes) files) bitsizes

makeAllTasks :: FilePath -> FilePath -> [Task]
makeAllTasks srcPath testPath = addModuleTasks srcPath $
  makeTasks' (srcPath </> "unsigned") testPath unsignedFiles ++
  makeTasks' (srcPath </> "signed")   testPath signedFiles

main :: IO ()
main =
  do args <- getArgs
     unless (length args == 1) $
       die ("generation takes exactly one argument, the target directory")
     let topLevel = head args
         srcPath = topLevel </> "src"
         testPath = topLevel </> "testdata"
         tasks = makeAllTasks srcPath testPath
         total = length tasks
     forM_ (zip [(1::Word)..] tasks) $ \ (i, task) ->
       do putStrLn ("[" ++ show i ++ "/" ++ show total ++ "] " ++ outputFile task)
          createDirectoryIfMissing True (takeDirectory (outputFile task))
          runGen (outputFile task) (fileGenerator task)