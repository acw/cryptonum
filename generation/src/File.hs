module File(
         File(..),
         Task(..),
         addModuleTasks,
         makeTasks
       )
 where

import Control.Monad(forM_)
import Data.Char(toUpper)
import qualified Data.Map.Strict as Map
import Gen(Gen,blank,out)
import System.FilePath(takeBaseName,takeDirectory,takeFileName,(</>))

data File = File {
    predicate :: Word -> [Word] -> Bool,
    outputName :: FilePath,
    generator :: Word -> Gen (),
    testGenerator :: Maybe (Word -> Gen ())
}

data Task = Task {
    outputFile :: FilePath,
    fileGenerator :: Gen ()
}

makeTasks :: FilePath -> FilePath ->
            Word -> [Word] ->
            File ->
            [Task]
makeTasks srcBase testBase size allSizes file
  | predicate file size allSizes =
      let base = Task (srcBase </> ("u" ++ show size) </> outputName file <> ".rs") (generator file size)
      in case testGenerator file of 
           Nothing -> [base]
           Just x ->
            [base, Task (testBase </> outputName file </> ("U" ++ show size ++ ".test")) (x size)]
  | otherwise = []

addModuleTasks :: FilePath -> [Task] -> [Task]
addModuleTasks base baseTasks = unsignedTask : (baseTasks ++ moduleTasks)
 where
  moduleMap = foldr addModuleInfo Map.empty baseTasks
  addModuleInfo task =
    Map.insertWith (++) (takeDirectory (outputFile task))
                        [takeBaseName (outputFile task)]
  moduleTasks = Map.foldrWithKey generateModuleTask [] moduleMap
  generateModuleTask directory mods acc = acc ++ [Task {
      outputFile = directory </> "mod.rs",
      fileGenerator =
        do forM_ mods $ \ modle -> out ("mod " ++ modle ++ ";")
           blank
           out ("pub use base::" ++ upcase (takeFileName directory) ++ ";")
  }]
  unsignedTask = Task {
      outputFile = base </> "unsigned" </> "mod.rs",
      fileGenerator =
        do forM_ (Map.keys moduleMap) $ \ key ->
             out ("mod " ++ takeFileName key ++ ";")
           blank
           forM_ (Map.keys moduleMap) $ \ key ->
             out ("pub use " ++ takeFileName key ++ "::" ++
                  upcase (takeFileName key) ++ ";")
  }
 
upcase :: String -> String
upcase = map toUpper