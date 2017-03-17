module Crawler.Options where

import Data.Char (toUpper)
import Data.Maybe (fromMaybe)

import System.Console.GetOpt

import Git.Types

-- | Options available to the Crawler.
data Opts
  = Opts
    { optDiff     :: DiffAlgo
    , optVerb     :: Bool
    , optCtrs     :: Bool
    , optOutput   :: Maybe FilePath
    , optInput    :: String
    , optHeader   :: Bool
    , optColBased :: Bool
    , optASTLCS   :: Bool
    } deriving Show

defaultOpts :: String -> Opts
defaultOpts n = Opts Myers False False Nothing n Nothing False False False

options :: [OptDescr (Opts -> Opts)]
options =
  [ Option ['d'] ["diff-algo"]
      (ReqArg (\f opts -> opts { optDiff = maybe Myers id (readDiffAlgo f)})
      "patience|histogram")
      "\nChoose the diff algorithm to use with 'diff log'\n"
  , Option ['o']     ["output"]
       (OptArg ((\ f opts -> opts { optOutput = Just f }) . fromMaybe "output")
                "FILE")
        "\noutput FILE, stdout by default\n"
  , Option ['v']     ["verbose"]
        (NoArg (\ opts -> opts { optVerb = True }))
        "\nRuns verbose\n"
  , Option ['h']     ["header"]
        (NoArg (\ opts -> opts { optHeader = True }))
        "\nShows the header of the columns\n"
  , Option ['A']     ["ast"]
        (NoArg (\ opts -> opts { optCtrs = True, optASTLCS = True }))
        "\nWARNING: SLOW\nCompute numbers for AST diff index\ninstead of Token diff index.\nImplies -c\n"
  , Option ['c']     ["constructor"]
        (NoArg (\ opts -> opts { optCtrs = True }))
        "\nRuns constructor analisys\n"
  , Option []     ["use-columns"]
        (NoArg (\ opts -> opts { optColBased = True }))
        "\nUse columns to refine constructor analisys\n"
{-
  , Option ['C']     ["only-constructor"]
        (ReqArg (\f opts -> opts { optOnlyCtrs = Just f }) "n[,c]:m")
        "\nRuns ONLY constructor analisys.\nInput should then be <commit>:file\nInput 14,5:34 means 'Constructors on line 14 from column 5, to line 34'\nInput l20:30 means 'Constructors from line 20 to lines 30'\n"
-}
  ]

compileOpts :: [String] -> IO Opts
compileOpts argv =
   case getOpt Permute options argv of
      (o,[n],[]  ) -> return (foldl (flip id) (defaultOpts n) o)
      (_,_,errs)   -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: hsglc crawl [OPTION...] <file.hs>"
