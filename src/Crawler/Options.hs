module Crawler.Options (Opts(..) , compileOpts) where

import Data.Char (toUpper)
import Data.Maybe (fromMaybe)

import System.Console.GetOpt

import Git.Types

-- | Options available to the Crawler.
data Opts
  = Opts
    { optDiffAlgo     :: DiffAlgo
    , optVerb         :: Bool
    , optInput        :: String
    , optHeader       :: Bool
    , optColBased     :: Bool
    , optKeepNoChg    :: Bool
    } deriving Show

defaultOpts :: String -> Opts
defaultOpts n = Opts Myers False n False True False

options :: [OptDescr (Opts -> Opts)]
options =
  [ Option ['d'] ["diff-algorithm"]
      (ReqArg (\f opts -> opts { optDiffAlgo = maybe Myers id (readDiffAlgo f)})
      "")
      "\nChoose the diff algorithm to use with 'diff log'.\nAny option that can be passed to git's '--diff-algorithm' work.\n"
  , Option ['v']     ["verbose"]
        (NoArg (\ opts -> opts { optVerb = True }))
        "\nRuns verbose.\n"
  , Option ['h']     ["header"]
        (NoArg (\ opts -> opts { optHeader = True }))
        "\nShows the header of the columns.\n"
  , Option []     ["dont-use-columns"]
        (NoArg (\ opts -> opts { optColBased = False }))
        "\nDo not use columns to refine constructor analisys.\n"
  , Option ['k']     ["keep-no-change"]
        (NoArg (\ opts -> opts { optKeepNoChg = True }))
        "\nKeeps the edits with only insertions or deletions in the data.\n"
{-
  , Option ['c']     ["changed-constructor"]
        (NoArg (\ opts -> opts { optGetChgdCon = True }))
        "\nShows the constructors that changed, intead of only\nthe first common parent.\n"
-}
  ]

compileOpts :: [String] -> IO Opts
compileOpts argv =
   case getOpt Permute options argv of
      (o,[n],[]  ) -> return (foldl (flip id) (defaultOpts n) o)
      (_,_,errs)   -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: hsglc crawl [OPTION...] <file.hs>"
