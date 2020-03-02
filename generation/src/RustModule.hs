{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes  #-}
module RustModule(
         RustModule(..),
         Task(..),
         generateTasks,
         testFile
       )
 where

import Control.Monad(forM_, unless)
import Data.Char(toUpper)
import Data.List(isPrefixOf, partition)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import Data.Maybe(mapMaybe)
import Language.Rust.Data.Ident(mkIdent)
import Language.Rust.Data.Position(Span, spanOf)
import Language.Rust.Pretty(writeSourceFile)
import Language.Rust.Quote(item, sourceFile)
import Language.Rust.Syntax(Item(..), SourceFile(..), Visibility(..))
import System.CPUTime(getCPUTime)
import System.IO(Handle,hPutStrLn)
import System.Random(RandomGen(..))

minimumTestCases :: Int
minimumTestCases = 10

maximumTestCases :: Int
maximumTestCases = 5000

targetTestGenerationTime :: Float
targetTestGenerationTime = 2.0 -- in seconds

targetTestGenerationPicos :: Integer
targetTestGenerationPicos =
  floor (targetTestGenerationTime * 1000000000000.0)

data RustModule = RustModule {
    predicate :: Word -> [Word] -> Bool,
    outputName :: String,
    isUnsigned :: Bool,
    generator :: Word -> [Word] -> SourceFile Span,
    testCase :: forall g. RandomGen g => Maybe (Word -> g -> (Map String String, g))
}

data Task = Task {
    outputFile :: FilePath,
    writer     :: Handle -> IO ()
}

testFile :: Bool -> Word -> FilePath
testFile True  size = "U" ++ show5 size ++ ".test"
testFile False size = "I" ++ show5 size ++ ".test"

show5 :: Word -> String
show5 = go . show
 where
  go x | length x < 5 = go ('0' : x)
       | otherwise    = x

generateTasks :: RandomGen g => g -> [RustModule] -> [Word] -> [Task]
generateTasks rng modules sizes = allTheFiles
 where
   allTheFiles = implementationsAndTests ++
                 [lump "src/signed", lump "src/unsigned"]
   implementationsAndTests = concatMap generateModules sizes
   --
   lump prefix =
     let allFiles = map outputFile implementationsAndTests
         files = filter (prefix `isPrefixOf`) allFiles
         moduleFiles = map (drop (length prefix + 1)) files
         moduleNames = map (takeWhile (/= '.')) moduleFiles 
         moduleIdents = map mkIdent moduleNames
         types = map (mkIdent . map toUpper) moduleNames
         mods = map (\ name -> [item| mod $$name; |]) moduleIdents
         uses = zipWith (\ mname tname -> [item| pub use $$mname::$$tname; |])
                        moduleIdents types
         file = [sourceFile| $@{mods} $@{uses} |]
      in Task (prefix ++ ".rs") (\hndl -> writeSourceFile hndl file)
   --
   generateModules size =
     let modules' = filter (\m -> predicate m size sizes) modules
         (umodules, smodules) = partition isUnsigned modules'
         unsignedTasks = generateImplementations "U" size umodules
         signedTasks = generateImplementations "I" size smodules
     in unsignedTasks ++ signedTasks ++ mapMaybe (generateTests size rng) modules'
   -- 
   generateImplementations startsWith size modules'
     | null modules' = []
     | otherwise =
         let name = mkIdent (startsWith ++ show size)
             baseInclude = [item| pub use self::base::$$name; |]
             moduleSources = map (generateSubmodule size sizes) modules'
             moduleFile | startsWith == "I" = "src/signed/i" ++ show size ++ ".rs"
                        | otherwise         = "src/unsigned/u" ++ show size ++ ".rs"
             allSource = SourceFile Nothing [] (baseInclude : moduleSources)
         in [Task moduleFile (\ hndl -> writeSourceFile hndl allSource)]

generateSubmodule :: Word -> [Word] -> RustModule -> Item Span
generateSubmodule size allSizes m =
  let SourceFile _ attrs internals = generator m size allSizes
      modName = mkIdent (outputName m)
      modSpan = spanOf internals
  in Mod attrs CrateV modName (Just internals) modSpan

generateTests :: RandomGen g =>
                 Word -> g ->
                 RustModule ->
                 Maybe Task
generateTests size rng m = fmap builder (testCase m)
 where
  builder testGenerator =
    let outFile = "testdata/" ++ outputName m ++ "/" ++ testFile (isUnsigned m) size
        testGenAction hndl = writeTestCases hndl (snd (split rng)) (testGenerator size)
    in Task outFile testGenAction

writeTestCases :: RandomGen g =>
                  Handle -> g ->
                  (g -> (Map String String, g)) ->
                  IO ()
writeTestCases hndl rng nextTest =
  do startTime <- getCPUTime
     let stopTime = startTime + targetTestGenerationPicos
     go 0 stopTime rng
 where
  go x endTime g
   | x >= maximumTestCases = return ()
   | x <  minimumTestCases = emit x endTime g
   | otherwise =
       do now <- getCPUTime
          unless (now >= endTime) $
            emit x endTime g
  --
  emit x endTime g =
    do let (test, g') = nextTest g
       writeTestCase hndl test
       go (x + 1) endTime g'

writeTestCase :: Handle -> Map String String -> IO ()
writeTestCase hndl test =
  forM_ (Map.toList test) $ \ (key, value) ->
    hPutStrLn hndl (key ++ ": " ++ value)
