{-# LANGUAGE QuasiQuotes #-}
module File(
         File(..),
         Task(..),
         addModuleTasks,
         makeTask
       )
 where

import Data.Char(toUpper)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import System.FilePath(takeBaseName,takeDirectory,takeFileName,(</>))
import Language.Rust.Data.Ident(Ident,mkIdent)
import Language.Rust.Data.Position(Span)
import Language.Rust.Quote(item,sourceFile)
import Language.Rust.Syntax(Item,SourceFile)

data File = File {
    predicate :: Word -> [Word] -> Bool,
    outputName :: FilePath,
    generator :: Word -> SourceFile Span
}

data Task = Task {
    outputFile :: FilePath,
    fileData :: SourceFile Span
}

makeTask :: FilePath ->
            Word -> [Word] ->
            File ->
            Maybe Task
makeTask base size allSizes file
  | predicate file size allSizes =
     Just Task {
         outputFile = base </> ("u" ++ show size) </> outputName file <> ".rs",
         fileData = generator file size
     }
  | otherwise =
     Nothing

addModuleTasks :: FilePath -> [Task] -> [Task]
addModuleTasks base baseTasks = unsignedTask : (baseTasks ++ moduleTasks)
 where
  moduleMap :: Map String [String]
  moduleMap = foldr addModuleInfo Map.empty baseTasks

  addModuleInfo :: Task -> Map String [String] -> Map String [String]
  addModuleInfo task =
    Map.insertWith (++) (takeDirectory (outputFile task))
                        [takeBaseName (outputFile task)]

  moduleTasks :: [Task]
  moduleTasks = Map.foldrWithKey generateModuleTask [] moduleMap

  generateModuleTask :: String -> [String] -> [Task] -> [Task]
  generateModuleTask directory mods acc = acc ++ [Task {
      outputFile = directory </> "mod.rs",
      fileData =
        let modules = map (buildModule . mkIdent) mods
            user = mkIdent (upcase (takeFileName directory))
        in [sourceFile|
             $@{modules}
             pub use base::$$user;
           |]
  }]

  unsignedTask :: Task
  unsignedTask = Task {
      outputFile = base </> "unsigned" </> "mod.rs",
      fileData =
        let modules = map (buildModule . mkIdent . takeFileName) (Map.keys moduleMap)
            uses = map (buildUse . takeFileName) (Map.keys moduleMap)
        in [sourceFile|
             $@{modules}
             $@{uses}
           |]
  }

buildModule :: Ident -> Item Span
buildModule x = [item| mod $$x; |]

buildUse :: String -> Item Span
buildUse x =
  let base = mkIdent x
      up = mkIdent (upcase x)
  in [item| pub use $$base::$$up; |]

upcase :: String -> String
upcase = map toUpper
