module Crunch.Options (Opts(..) , compileOpts) where

import Data.Char (toUpper)
import Data.Maybe (fromMaybe)

import System.Console.GetOpt

-- | Options available to the Crawler.
data Opts
  = Opts
    { optMinDIdx  :: Float
    , optMaxDIdx  :: Float
    , optUseTok   :: Bool
    } deriving Show

defaultOpts :: Opts
defaultOpts = Opts 0.0 1.0 False

options :: [OptDescr (Opts -> Opts)]
options =
  [ Option ['m'] ["min-idx"]
      (ReqArg (\f opts -> opts { optMinDIdx = read f})
      "(0 .. 1)")
      "\nMinimum difference-index to consider\n"
  , Option ['M'] ["max-idx"]
      (ReqArg (\f opts -> opts { optMaxDIdx = read f})
      "(0 .. 1)")
      "\nMaximum difference-index to consider\n"
  , Option ['t']     ["token"]
        (NoArg (\ opts -> opts { optUseTok = True }))
        "\nUse Token difference index instead of AST.\n"
  ]

compileOpts :: [String] -> IO (Opts , [String])
compileOpts argv =
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOpts o , n)
      (_,_,errs)   -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: hsglc crawl [OPTION...] <file.hs>"
