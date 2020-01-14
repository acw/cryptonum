{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes  #-}
module File(
         File(..),
         Task(..),
         generateTasks,
         testFile
       )
 where

import Control.Monad(forM_)
import Data.Char(toUpper)
import Data.List(isPrefixOf)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import Language.Rust.Data.Ident(mkIdent)
import Language.Rust.Data.Position(Span)
import Language.Rust.Pretty(writeSourceFile)
import Language.Rust.Quote(item,sourceFile)
import Language.Rust.Syntax(SourceFile)
import System.FilePath(dropExtension,splitFileName,takeFileName,(</>))
import System.IO(Handle,hPutStrLn)
import System.Random(RandomGen(..))

data File = File {
    predicate :: Word -> [Word] -> Bool,
    outputName :: FilePath,
    isUnsigned :: Bool,
    generator :: Word -> [Word] -> SourceFile Span,
    testCase :: forall g. RandomGen g => Maybe (Word -> g -> [Map String String])
}

data Task = Task {
    outputFile :: FilePath,
    writer :: Handle -> IO ()
}

testFile :: Bool -> Word -> FilePath
testFile True  size = "U" ++ show5 size ++ ".test"
testFile False size = "I" ++ show5 size ++ ".test"

show5 :: Word -> String
show5 = go . show
 where
  go x | length x < 5 = go ('0' : x)
       | otherwise    = x

generateTasks :: RandomGen g => g -> [File] -> [Word] -> [Task]
generateTasks rng files sizes = basicTasks ++ moduleTasks
 where
  basicTasks = go rng files sizes
  moduleTasks = generateModules basicTasks
  --
  go :: RandomGen g => g -> [File] -> [Word] -> [Task]
  go _ []             _  = []
  go g (_:rest)       [] = go g rest sizes
  go g files'@(file:_) (size:rest)
    | not (predicate file size sizes) = go g files' rest
    | otherwise =
        let (myg, theirg) = split g
            tasks = go theirg files' rest
            (signedBit, prefix) | isUnsigned file = ("unsigned", "u")
                                | otherwise       = ("signed", "i")
            mainTask = Task {
              outputFile = "src" </> signedBit </> (prefix ++ show size) </>
                           outputName file ++ ".rs",
              writer = \ hndl -> writeSourceFile hndl (generator file size sizes)
            }
        in case testCase file of
             Nothing ->
               mainTask : tasks
             Just caseGenerator ->
               let testTask = Task {
                     outputFile = "testdata" </> outputName file </> testFile (isUnsigned file) size,
                     writer = \ hndl -> writeTestCase hndl (caseGenerator size myg)
                   }
               in testTask : mainTask : tasks

generateModules :: [Task] -> [Task]
generateModules tasks = Map.foldrWithKey maddModule [] fileMap ++ [signedTask, unsignedTask]
 where
  maddModule path mods acc
   | ("src/unsigned" `isPrefixOf` path) || ("src/signed" `isPrefixOf` path) =
        let (basePath, lowerName) = splitFileName (init path)
            upperName = map toUpper lowerName
            task = Task {
              outputFile = basePath </> lowerName ++ ".rs",
              writer = \ hndl ->
                do forM_ mods $ \ modl ->
                     hPutStrLn hndl ("mod " ++ modl ++ ";")
                   hPutStrLn hndl ("pub use base::" ++ upperName ++ ";")
            }
        in task : acc
   | otherwise =
        acc
  fileMap = foldr buildBaseMap Map.empty tasks
  buildBaseMap task acc =
    let (dir, fileext) = splitFileName (outputFile task)
        file = dropExtension fileext
    in Map.insertWith (++) dir [file] acc
  --
  signedTask = moduleTask "signed"
  unsignedTask = moduleTask "unsigned"
  moduleTask kind =
    let mods = Map.foldrWithKey (topModule kind) [] fileMap
        pubuses = Map.foldrWithKey (pubUse kind) [] fileMap
    in Task {
         outputFile = "src" </> (kind ++ ".rs"),
         writer = \ hndl ->
           writeSourceFile hndl [sourceFile|
             $@{mods}
             $@{pubuses}
          |]
       }
  topModule kind path _ acc
   | ("src/" ++ kind) `isPrefixOf` path =
        let lowerName = takeFileName (init path)
            modl = mkIdent lowerName
        in [item| mod $$modl; |] : acc
   | otherwise =
        acc
  pubUse kind path _ acc
   | ("src/" ++ kind) `isPrefixOf` path =
        let lowerName = takeFileName (init path)
            tname = mkIdent (map toUpper lowerName)
            modl = mkIdent lowerName
        in [item| pub use $$modl::$$tname; |] : acc
   | otherwise =
        acc


writeTestCase :: Handle -> [Map String String] -> IO ()
writeTestCase hndl tests =
  forM_ tests $ \ test ->
    forM_ (Map.toList test) $ \ (key, value) ->
      hPutStrLn hndl (key ++ ": " ++ value)
