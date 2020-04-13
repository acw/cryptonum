module Main
 where

import Add(safeAddOps,unsafeAddOps,safeSignedAddOps,unsafeSignedAddOps)
import Base(base)
import BinaryOps(binaryOps)
import Compare(comparisons, signedComparisons)
import Control.Concurrent(forkFinally)
import Control.Concurrent.MVar(MVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Monad(replicateM, void)
import Conversions(conversions, signedConversions)
import CryptoNum(cryptoNum)
import Control.Monad(forM_,unless)
import Data.List(nub)
import Data.Text.Lazy(Text, pack)
import Division(divisionOps)
import GHC.Conc(getNumCapabilities)
import ModInv(generateModInvOps)
import ModOps(modulusOps)
import Multiply(safeMultiplyOps, unsafeMultiplyOps)
import RustModule(RustModule(suggested),Task(..),generateTasks)
import Scale(safeScaleOps, unsafeScaleOps)
import Shift(shiftOps, signedShiftOps)
import Signed(signedBaseOps)
import Subtract(safeSubtractOps,unsafeSubtractOps,safeSignedSubtractOps,unsafeSignedSubtractOps)
import System.Directory(createDirectoryIfMissing)
import System.Environment(getArgs)
import System.Exit(die)
import System.FilePath(takeDirectory,(</>))
import System.IO(IOMode(..),withFile)
import System.ProgressBar(Label(..), Progress(..), ProgressBar, Timing, defStyle, newProgressBar, stylePrefix, updateProgress)
import System.Random(getStdGen)

rsaWordSizes :: [Word]
rsaWordSizes = [512, 1024, 2048, 3072, 4096, 8192, 15360]

dsaWordSizes :: [Word]
dsaWordSizes = [192, 256, 384, 1024, 2048, 3072]

ecdsaIntSizes :: [Word]
ecdsaIntSizes = [192, 256, 384, 576]

bitsizes :: [Word]
bitsizes = expandSizes initialSet
 where
  initialSet = nub (rsaWordSizes ++ dsaWordSizes ++ ecdsaIntSizes)

unsignedFiles :: [RustModule]
unsignedFiles = [
    base
  , binaryOps
  , comparisons
  , conversions
  , cryptoNum
  , divisionOps
  , generateModInvOps
  , modulusOps
  , safeAddOps
  , safeMultiplyOps
  , safeScaleOps
  , safeSubtractOps
  , shiftOps
  , unsafeAddOps
  , unsafeMultiplyOps
  , unsafeScaleOps
  , unsafeSubtractOps
  ]

signedFiles :: [RustModule]
signedFiles = [
    safeSignedAddOps
  , safeSignedSubtractOps
  , signedBaseOps
  , signedComparisons
  , signedConversions
  , signedShiftOps
  , unsafeSignedAddOps
  , unsafeSignedSubtractOps
  ]

allFiles :: [RustModule]
allFiles = unsignedFiles ++ signedFiles

expandSizes :: [Word] -> [Word]
expandSizes ls = bigger
 where
  bigger = nub (ls ++ concatMap (\ f -> concatMap (\ x -> suggested f x) ls) allFiles)

printLast :: Progress String -> Timing -> Text
printLast prog _ = pack (progressCustom prog)

runThread :: ProgressBar String -> FilePath -> MVar [Task] -> IO (MVar ())
runThread pb outputPath mtaskls =
  do res <- newEmptyMVar
     void $ forkFinally step (threadDie res)
     return res
 where
   step =
     do tasks <- takeMVar mtaskls
        case tasks of
          [] ->
            putMVar mtaskls []
          task : rest ->
            do putMVar mtaskls rest
               let target = outputPath </> outputFile task
               createDirectoryIfMissing True (takeDirectory target)
               withFile target WriteMode $ \ targetHandle ->
                 writer task targetHandle
               updateProgress pb (\ p -> p{ progressCustom = outputFile task,
                                            progressDone   = progressDone p + 1 })
               step
   threadDie resmv thrRes =
     do case thrRes of
          Left se -> putStrLn ("Thread died: " ++ show se)
          Right () -> return ()
        putMVar resmv ()

main :: IO ()
main =
  do args <- getArgs
     unless (length args == 1) $
       die ("generation takes exactly one argument, the target directory")
     g <- getStdGen
     let style = defStyle{ stylePrefix = Label printLast }
         allTasks = generateTasks g allFiles bitsizes
         progress = Progress 0 total "starting"
         total = length allTasks
     pb <- newProgressBar style 60 progress
     chan <- newMVar allTasks
     count <- getNumCapabilities
     threads <- replicateM count (runThread pb (head args) chan)
     forM_ threads (\ m -> takeMVar m)
