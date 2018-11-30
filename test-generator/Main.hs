{-# LANGUAGE LambdaCase #-}
import           Control.Concurrent(forkIO)
import           Control.Concurrent.Chan(Chan,newChan,readChan,writeChan)
import           Control.Concurrent.MVar(MVar,newMVar,modifyMVar)
import           Control.Exception(SomeException,catch)
import           Control.Monad(foldM,foldM_,forM_,replicateM_,void)
import qualified Data.Map.Strict as Map
import           Database(Database, emptyDatabase)
import           GHC.Conc(getNumCapabilities)
import           Requirements(Requirement(Req),requirements)
import           System.Console.AsciiProgress
import           System.Directory(createDirectoryIfMissing,doesFileExist)
import           System.FilePath(takeDirectory)
import           System.IO(Handle,IOMode(..),hPutStrLn,withFile)
import           System.Random(StdGen,getStdGen,split)
import           Tests(SizedTest,testDatabase)

data Task = Task {
    taskName  :: String,
    taskFile  :: FilePath,
    taskTest  :: SizedTest,
    taskCount :: Int
}

requirementToTasks :: Requirement -> [Task]
requirementToTasks (Req size operation) =
    let tests = filter (\ (op,_,_,_) -> op == operation) testDatabase
    in map translate tests
 where
  translate (_, dir, name, test) = Task {
      taskName   = show size ++ "-bit " ++ name
    , taskFile   = "../testdata/" ++ dir ++ "/" ++ (pad 5 '0' (show size)) ++ ".test"
    , taskTest   = test size
    , taskCount  = 1000
  }

runTask :: StdGen -> Task -> IO StdGen
runTask gen task =
  do createDirectoryIfMissing True (takeDirectory (taskFile task))
     alreadyDone <- doesFileExist (taskFile task)
     if alreadyDone
        then return gen
        else withFile (taskFile task) WriteMode $ \ hndl ->
               do pg <- newProgressBar def{ pgOnCompletion = Just ("Finished " ++ taskName task),
                                            pgFormat = taskName task ++ " " ++ pgFormat def,
                                            pgTotal = fromIntegral (taskCount task) }
                  let initval = emptyDatabase gen
                  (_, gen') <- foldM (writer hndl pg (taskTest task)) initval [0..taskCount task]
                  return gen'
 where
  writer :: Handle -> ProgressBar -> SizedTest -> Database -> Int -> IO Database
  writer hndl pg runner db x =
    do let (output, key, acc@(db',gen')) = runner db
           before = Map.findWithDefault [] "RESULT" db'
       if length (filter (== key) before) >= 10
          then writer hndl pg runner acc x
          else do forM_ (Map.toList output) $ \ (outkey, val) ->
                    hPutStrLn hndl (outkey ++ ": " ++ val)
                  tick pg
                  return (Map.insert "RESULT" (key : before) db', gen')

pad :: Int -> Char -> String -> String
pad x c str | length str < x = pad x c (c : str)
            | otherwise      = str

taskExecutor :: MVar [Task] -> Chan () -> StdGen -> IO StdGen
taskExecutor taskList done gen =
    do mnext <- modifyMVar taskList (\case
                                       [] -> return ([], Nothing)
                                       (x:xs) -> return (xs, Just x))
       case mnext of
         Nothing -> do writeChan done ()
                       return gen
         Just x  -> do gen' <- runTask gen x
                       taskExecutor taskList done gen'

spawnExecutor :: MVar [Task] -> Chan () -> StdGen -> Int -> IO StdGen
spawnExecutor tasks done gen0 _ =
  do let (gen1, gen2) = split gen0
     void (forkIO (catch (void (taskExecutor tasks done gen1)) handler))
     return gen2
 where
  handler :: SomeException -> IO ()
  handler e = putStrLn ("ERROR: " ++ show e)

main :: IO ()
main = displayConsoleRegions $
    do 
       executors <- getNumCapabilities
       done <- newChan
       gen <- getStdGen
       tasks <- newMVar (concatMap requirementToTasks requirements)
       foldM_ (spawnExecutor tasks done) gen [1..executors]
       replicateM_ executors (void $ readChan done)