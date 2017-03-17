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
    , optKeep0Cluster :: Bool
    } deriving Show

defaultOpts :: String -> Opts
defaultOpts n = Opts Myers False n False False False

options :: [OptDescr (Opts -> Opts)]
options =
  [ Option ['d'] ["diff-algo"]
      (ReqArg (\f opts -> opts { optDiffAlgo = maybe Myers id (readDiffAlgo f)})
      "patience|histogram")
      "\nChoose the diff algorithm to use with 'diff log'\n"
  , Option ['v']     ["verbose"]
        (NoArg (\ opts -> opts { optVerb = True }))
        "\nRuns verbose\n"
  , Option ['h']     ["header"]
        (NoArg (\ opts -> opts { optHeader = True }))
        "\nShows the header of the columns\n"
  , Option []     ["use-columns"]
        (NoArg (\ opts -> opts { optColBased = True }))
        "\nUse columns to refine constructor analisys\n"
  , Option ['k']     ["keep-0-cluster"]
        (NoArg (\ opts -> opts { optKeep0Cluster = True }))
        "\nKeeps the edits with zero clusters in the data\n"
  ]

compileOpts :: [String] -> IO Opts
compileOpts argv =
   case getOpt Permute options argv of
      (o,[n],[]  ) -> return (foldl (flip id) (defaultOpts n) o)
      (_,_,errs)   -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: hsglc crawl [OPTION...] <file.hs>"
