{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes  #-}
module RustModule(
         RustModule(..),
         Task(..),
         generateTasks,
         testFile
       )
 where

import Control.Monad(forM_)
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
import System.IO(Handle,hPutStrLn)
import System.Random(RandomGen(..))

data RustModule = RustModule {
    predicate :: Word -> [Word] -> Bool,
    outputName :: String,
    isUnsigned :: Bool,
    generator :: Word -> [Word] -> SourceFile Span,
    testCase :: forall g. RandomGen g => Maybe (Word -> g -> [Map String String])
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
  builder testGeneration =
    let outFile = "testdata/" ++ outputName m ++ "/" ++ testFile (isUnsigned m) size
        testGenAction hndl = writeTestCase hndl (testGeneration size (snd (split rng)))
    in Task outFile testGenAction

--   basicTasks ++ moduleTasks
-- where
--  basicTasks = go rng files sizes
--  moduleTasks = generateModules basicTasks
--  --
--  go :: RandomGen g => g -> [RustModule] -> [Word] -> [Task]
--  go _ []             _  = []
--  go g (_:rest)       [] = go g rest sizes
--  go g files'@(file:_) (size:rest)
--    | not (predicate file size sizes) = go g files' rest
--    | otherwise =
--        let (myg, theirg) = split g
--            tasks = go theirg files' rest
--            (signedBit, prefix) | isUnsigned file = ("unsigned", "u")
--                                | otherwise       = ("signed", "i")
--            mainTask = Task {
--              outputFile = "src" </> signedBit </> (prefix ++ show size) </>
--                           outputName file ++ ".rs",
--              writer = \ hndl -> writeSourceFile hndl (generator file size sizes)
--            }
--        in case testCase file of
--             Nothing ->
--               mainTask : tasks
--             Just caseGenerator ->
--               let testTask = Task {
--                     outputFile = "testdata" </> outputName file </> testFile (isUnsigned file) size,
--                     writer = \ hndl -> writeTestCase hndl (caseGenerator size myg)
--                   }
--               in testTask : mainTask : tasks
--
--generateModules :: [Task] -> [Task]
--generateModules tasks = Map.foldrWithKey maddModule [] fileMap ++ [signedTask, unsignedTask]
-- where
--  maddModule path mods acc
--   | ("src/unsigned" `isPrefixOf` path) || ("src/signed" `isPrefixOf` path) =
--        let (basePath, lowerName) = splitFileName (init path)
--            upperName = map toUpper lowerName
--            task = Task {
--              outputFile = basePath </> lowerName ++ ".rs",
--              writer = \ hndl ->
--                do forM_ mods $ \ modl ->
--                     hPutStrLn hndl ("mod " ++ modl ++ ";")
--                   hPutStrLn hndl ("pub use base::" ++ upperName ++ ";")
--            }
--        in task : acc
--   | otherwise =
--        acc
--  fileMap = foldr buildBaseMap Map.empty tasks
--  buildBaseMap task acc =
--    let (dir, fileext) = splitFileName (outputFile task)
--        file = dropExtension fileext
--    in Map.insertWith (++) dir [file] acc
--  --
--  signedTask = moduleTask "signed"
--  unsignedTask = moduleTask "unsigned"
--  moduleTask kind =
--    let mods = Map.foldrWithKey (topModule kind) [] fileMap
--        pubuses = Map.foldrWithKey (pubUse kind) [] fileMap
--    in Task {
--         outputFile = "src" </> (kind ++ ".rs"),
--         writer = \ hndl ->
--           writeSourceFile hndl [sourceFile|
--             $@{mods}
--             $@{pubuses}
--          |]
--       }
--  topModule kind path _ acc
--   | ("src/" ++ kind) `isPrefixOf` path =
--        let lowerName = takeFileName (init path)
--            modl = mkIdent lowerName
--        in [item| mod $$modl; |] : acc
--   | otherwise =
--        acc
--  pubUse kind path _ acc
--   | ("src/" ++ kind) `isPrefixOf` path =
--        let lowerName = takeFileName (init path)
--            tname = mkIdent (map toUpper lowerName)
--            modl = mkIdent lowerName
--        in [item| pub use $$modl::$$tname; |] : acc
--   | otherwise =
--        acc
--
--
writeTestCase :: Handle -> [Map String String] -> IO ()
writeTestCase hndl tests =
  forM_ tests $ \ test ->
    forM_ (Map.toList test) $ \ (key, value) ->
      hPutStrLn hndl (key ++ ": " ++ value)
