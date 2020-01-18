module Main
 where

import Add(safeAddOps,unsafeAddOps,safeSignedAddOps,unsafeSignedAddOps)
import Base(base)
import BinaryOps(binaryOps)
import Compare(comparisons, signedComparisons)
import Conversions(conversions, signedConversions)
import CryptoNum(cryptoNum)
import Control.Monad(forM_,unless)
import Division(divisionOps)
import File(File,Task(..),generateTasks)
import ModInv(generateModInvOps)
import ModOps(modulusOps)
import Multiply(safeMultiplyOps, unsafeMultiplyOps)
import Scale(safeScaleOps, unsafeScaleOps)
import Shift(shiftOps, signedShiftOps)
import Signed(signedBaseOps)
import Subtract(safeSubtractOps,unsafeSubtractOps,safeSignedSubtractOps,unsafeSignedSubtractOps)
import System.Directory(createDirectoryIfMissing)
import System.Environment(getArgs)
import System.Exit(die)
import System.FilePath(takeDirectory,(</>))
import System.IO(IOMode(..),withFile)
import System.Random(getStdGen)

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
  , divisionOps
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

signedFiles :: [File]
signedFiles = [
    generateModInvOps
  , safeSignedAddOps
  , safeSignedSubtractOps
  , signedBaseOps
  , signedComparisons
  , signedConversions
  , signedShiftOps
  , unsafeSignedAddOps
  , unsafeSignedSubtractOps
  ]

allFiles :: [File]
allFiles = unsignedFiles ++ signedFiles

main :: IO ()
main =
  do args <- getArgs
     unless (length args == 1) $
       die ("generation takes exactly one argument, the target directory")
     g <- getStdGen
     let allTasks = generateTasks g allFiles bitsizes
         total = length allTasks
     forM_ (zip [(1::Word)..] allTasks) $ \ (i, task) ->
       do putStrLn ("[" ++ show i ++ "/" ++ show total ++ "] " ++ outputFile task)
          let target = head args </> outputFile task
          createDirectoryIfMissing True (takeDirectory target)
          withFile target WriteMode $ \ targetHandle ->
            writer task targetHandle
