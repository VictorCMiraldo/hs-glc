module Git.Process where

import Control.Monad.Except

import Git.Types

import System.Process
import System.Environment
import System.IO
import System.Exit

-- | given a filename, generate the list of options
--   we need to pass to 'git log' to obtain a parseable output.
gitLogOptionsFor :: String -> [String]
gitLogOptionsFor s = ["--pretty=format:%h" , "--follow", "-p", "--no-color", "--", s]

-- | Gets the log for a file with the default diff algorithm.
runGitLogDefault :: String -> IO (ExitCode , String , String)
runGitLogDefault file
  = readProcessWithExitCode "git" (gitLogOptionsFor file) ""

-- | Gets the log for a file with a given diff algorithm.
runGitLogWithAlgo :: DiffAlgo -> String -> IO (ExitCode , String , String)
runGitLogWithAlgo algo file
  = readProcessWithExitCode "git"
      ("log" : (show algo) : gitLogOptionsFor file) ""

-- | Runs 'git show' for a given file on a given revision.
--
--   Here, the argument must be on the format "abc123^:./path/to/file"
--
runGitShow :: String -> IO (ExitCode , String , String)
runGitShow hashfile
  = readProcessWithExitCode "git" ["show" , hashfile] ""

-- | Runs a 'git' process in a nice env.
runGit :: (MonadIO m , MonadError e m)
       => (ExitCode -> String -> e) -> m (ExitCode , String , String)
       -> m String
runGit errf act
  = do
      (exit , out , err) <- act
      if exit == ExitSuccess
      then return out
      else throwError (errf exit err)
    
