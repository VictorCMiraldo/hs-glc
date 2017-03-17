module Errors where

import Control.Monad.Except
import System.Exit

data Error
  = NoParse      String
  | NoTokenize   String
  | GitLogFail   ExitCode String
  | GitLogParse  String String
  | GitShowFail  ExitCode String
  | GitShowParse String String
  | NotImplemented
  deriving (Eq, Show)

type ErrIO = ExceptT Error IO

runErrIO :: ErrIO a -> IO a
runErrIO r
  = do
    res <- runExceptT r
    case res of
      Left err -> ioError (userError (show err))
      Right r  -> return r
  
