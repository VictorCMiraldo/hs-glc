module Main where

import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Lexer
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Extension

import Control.Monad
import Control.Exception

import Data.Algorithm.Diff
import Data.List (intersperse, groupBy)
import Data.Maybe (fromMaybe)
import Data.Char (toUpper)

import System.Process
import System.Environment
import System.IO
import System.Exit
import System.Console.GetOpt


import GitLog
import GetConstrs
  
-- * Main Function

-- Runs the full git log command, returns the exit code, stdout and stderr.
-- on a specific git algorithm.
data DiffAlgo
  = Default
  | Patience
  | Histogram
  deriving (Eq , Show , Read)


readGitLogFor :: Opts -> String -> IO (ExitCode , String , String)
readGitLogFor d file
  = do when (optVerb d) (hPutStrLn stderr $ "running: git " ++ unwords opts)
       readProcessWithExitCode "git" opts ""
  where
    toOpt Default = []
    toOpt Patience = ["--patience"]
    toOpt Histogram = ["--histogram"]
    
    opts = ["log", "--pretty=format:#%n%h", "--follow", "-p"
           ,"--no-color"] ++ toOpt (optDiff d) ++ ["--", file]

readGitRev :: Opts -> String -> IO (ExitCode , String , String)
readGitRev d hashfile
  = do when (optVerb d) (hPutStrLn stderr $ "running: git " ++ unwords opts)
       readProcessWithExitCode "git" opts ""
  where    
    opts = ["show", hashfile]

runExternal :: String -> IO (ExitCode , String , String) -> IO String
runExternal desc cmd
  = do
    (ex , out , err) <- cmd
    case ex of
      ExitSuccess -> return out
      _           -> ioError (userError ("runExternal '" ++ desc ++ "' : " ++ show ex))

-- ** Option Handling

data Opts
  = Opts
    { optDiff    :: DiffAlgo
    , optVerb    :: Bool
    , optCtrs    :: Bool
    , optOutput  :: Maybe FilePath
    , optInput   :: String
    , optOnlyCtrs :: Maybe String
    , optHeader  :: Bool
    , optColBased :: Bool
    , optASTLCS  :: Bool
    } deriving Show

defaultOpts :: String -> Opts
defaultOpts n = Opts Default False False Nothing n Nothing False False False

options :: [OptDescr (Opts -> Opts)]
options =
  [ Option ['d'] ["diff-algo"]
      (ReqArg (\f opts -> opts { optDiff = read (cap1 f)}) "patience|histogram")
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
  , Option ['C']     ["only-constructor"]
        (ReqArg (\f opts -> opts { optOnlyCtrs = Just f }) "n[,c]:m")
        "\nRuns ONLY constructor analisys.\nInput should then be <commit>:file\nInput 14,5:34 means 'Constructors on line 14 from column 5, to line 34'\nInput l20:30 means 'Constructors from line 20 to lines 30'\n"
  ]
  where
    cap1 [] = []
    cap1 (x:xs) = Data.Char.toUpper x : xs


compilerOpts :: [String] -> IO Opts
compilerOpts argv =
   case getOpt Permute options argv of
      (o,[n],[]  ) -> return (foldl (flip id) (defaultOpts n) o)
      (_,_,errs)   -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: hsFileHistCrunch [OPTION...] files..."

onlyCtrs :: Opts -> IO ()
onlyCtrs opts
  = do
    let Just lines = optOnlyCtrs opts
    let li         = parseLines lines
    file <- runExternal "git show" (readGitRev opts (optInput opts))
    mod  <- case myParser file of
                   ParseOk r -> return r
                   err       -> do when (optVerb opts) (hPutStrLn stderr "no parse")
                                   ioError (userError (show err))
    putStrLn $ unwords (constrsL li mod)
  where
    parseLines :: String -> LineInfo
    parseLines s = case readNumbers s of
                      [l , c , m] -> LineCol (l , c , m)
                      [l , m]     -> LineSpan (l , m)


isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False


-- Take cmd line arguments,
-- pass them around.
main :: IO ()
main = do
       -- Preprocess arguments
       args    <- getArgs
       opts    <- compilerOpts args

       if isJust (optOnlyCtrs opts)
       then onlyCtrs opts
       else do
         -- Opens output file, if any.
         output  <- maybe (return stdout)
                          (\s -> openFile s WriteMode)
                          (optOutput opts)

         when (optHeader opts) (hPutStrLn output (unwords showHeader))
         -- Runs the actual computation catching
         -- any exception we might throw.
         (run opts output) `catch` ((\_ -> hClose output) :: IOException -> IO ())
     where
       run opts output = do
         -- The actual code starts by parsing the git log for a given file:
         gitlog  <- parseLog <$> runExternal "git log"
                                   (readGitLogFor opts (optInput opts))

         -- XXX: Better to group the log by hash. We call git show less.
         -- Then we process all the entries in that log.
         mapM_ (processWithOut output opts) gitlog
         hClose output

showHeader :: [String]
showHeader = ["filename","hash"] ++ resultHeader

processWithOut :: Handle -> Opts -> GitLogChg -> IO ()
processWithOut outh opts gitlog@(GitLogChg hash _ _ _)
  = do res <- process opts gitlog
       mapM_ (maybe (return ()) shouldPrint) res
  where
    shouldPrint :: Result -> IO ()
    shouldPrint res
      | clusters res > 0 = hPutStrLn outh
                         $ unwords [quote (optInput opts), hash, show res]
      | otherwise        = return ()
      where
        quote s = '"':s ++ ['"']

-- * Processing everything

type HsModule = Module SrcSpanInfo

{- Parses a haskell module with a decent suite of extensions -}
myParser :: String -> ParseResult HsModule
myParser = parseModuleWithMode myMode
  where
    usefullExts :: [Extension]
    usefullExts = map EnableExtension
                [ MultiParamTypeClasses
                , FlexibleContexts
                , DefaultSignatures
                , TypeOperators
                ]

    myMode :: ParseMode
    myMode = defaultParseMode { baseLanguage = Haskell2010
                              , ignoreLanguagePragmas = False
                              , ignoreLinePragmas = False
                              , extensions = usefullExts
                              }

parsePrePos :: Opts -> GitLogChg -> IO (Maybe (HsModule , HsModule))
parsePrePos opts gitlog
  = (Just <$> parsePrePosCrash opts gitlog) `catch` saveMe
  where
    saveMe :: IOException -> IO (Maybe a)
    saveMe e = do when (optVerb opts) (hPutStrLn stderr ("Caught: " ++ show e))
                  return Nothing

    parsePrePosCrash :: Opts -> GitLogChg -> IO (HsModule , HsModule)
    parsePrePosCrash opts (GitLogChg hash fin fout chgs)
      = do
        let posFileName = hash ++  ":" ++ fout
        let preFileName = hash ++ "^:" ++ fin
        preFile <- runExternal "git show" (readGitRev opts preFileName)
        posFile <- runExternal "git show" (readGitRev opts posFileName)
        preModule <- case myParser preFile of
                             ParseOk r -> return r
                             err       -> do when (optVerb opts)
                                                  (hPutStrLn stderr "no parse preFile")
                                             ioError (userError (show err))
        posModule <- case myParser preFile of
                             ParseOk r -> return r
                             err       -> do when (optVerb opts)
                                                  (hPutStrLn stderr "no parse posFile")
                                             ioError (userError (show err))
        return (preModule , posModule)



tokenizeChange :: GitChange -> Either String ([Token] , [Token])
tokenizeChange (GitChange _ _ ins del)
  = let tks = ((,) <$> lexTokenStream (unlines ins)
                   <*> lexTokenStream (unlines del))
    in case tks of
      ParseFailed _ s     -> Left $ "tokenizer failed with: " ++ s
      ParseOk (tkI , tkD) -> Right (map unLoc tkI , map unLoc tkD)

type CInfo = Maybe (String , String)

data Result
  = Result { clusters  :: Int
           , metric    :: Metric
           , mli       :: Int
           , fromto    :: CInfo
           , lin       :: Int
           , lout      :: Int
           }

-- For processing a log entry, we first check if we want constructor information.
process :: Opts -> GitLogChg -> IO [Maybe Result]
process opts gitlog@(GitLogChg hash _ _ chgs)
-- if we do; we try to parse the modules.
  | optCtrs opts
    = do modules <- parsePrePos opts gitlog
         let processChgF = case modules of
                Nothing            -> processChg
                Just (preM , posM) -> processChgCtr preM posM
         mapM (processChgF opts) chgs
  | otherwise
    = mapM (processChg opts) chgs

processChgTkn :: Opts -> GitChange -> IO (Maybe Result)
processChgTkn opts chg
  = case tokenizeChange chg of
      -- If we can't tokenize, nothing to do here.
      Left err    -> when (optVerb opts) (hPutStrLn stderr err)
                  >> return Nothing
      -- Otherwise, we crunch tose numbers!
      Right (tI , tD) -> return . Just  $ crunchNumbers (getDiff tI tD)

processChg :: Opts -> GitChange -> IO (Maybe Result)
processChg opts chg@(GitChange preL posL ins del)
  = (annotateResult <$>) <$> processChgTkn opts chg
  where
    annotateResult re
      = re { lin  = preL
           , lout = posL
           , mli  = if length ins == length del then 0 else 1
           }

-- If option --constructor is enabled, annotate the result with
-- the first seen constructor.
processChgCtr :: HsModule -> HsModule -> Opts -> GitChange -> IO (Maybe Result)
processChgCtr preM posM opts chg@(GitChange preL posL ins del)
  = (annotateResult <$>)
  -- if we are using --ast level diff, skip the token LCS completely.
    <$> if optASTLCS opts
        then return (crunchNumbers <$> astDiff) 
        else processChg opts chg
  where
    firstMod (s:ss) (r:rr)
      | s == r    = 1 + firstMod ss rr
      | otherwise = 0
    firstMod _ _ = 0

    col = if length ins > 0 && length del > 0
          then firstMod (head del) (head ins)
          else 0

    -- If we are --use-columns , do so!
    preLS
      | optColBased opts = LineCol  (preL , col , preL + length del)
      | otherwise        = LineSpan (preL , preL + length del)
    posLS
      | optColBased opts = LineCol  (posL , col , posL + length del)
      | otherwise        = LineSpan (posL , posL + length ins)

    astDiff = let ci = constrsL preLS preM
                  co = constrsL posLS posM
               in if ci == [] || co == []
                  then Nothing
                  else Just (getDiff ci co)
    
    annotateResult re
      = re { fromto = (,) <$> (constrsM preLS preM) 
                          <*> (constrsM posLS posM)
           , lin  = preL
           , lout = posL
           , mli  = if length ins == length del then 0 else 1
           }

-- * Finally, some number crunching magic:


resultHeader :: [String]
resultHeader = ["clusters"] ++ metricHeader
               ++ ["multiline","ctrFrom","ctrTo","lineFrom","lineTo"]

instance Show Result where
  show (Result cl me mli Nothing li lo)
    = unwords [show cl, show me, show mli, "xxx" , "xxx" , show li , show lo]     
  show (Result cl me mli (Just (ci , cj)) li lo)
    = unwords [show cl, show me, show mli, ci , cj , show li , show lo]     

crunchNumbers :: [Diff a] -> Result
crunchNumbers dt = Result (getClusters dt) (count dt) 0 Nothing 0 0

getClusters :: [Diff a] -> Int
getClusters = length . filter isBothL . groupBy sameCons
  where
    sameCons (Both _ _) (Both _ _) = True
    sameCons (First _)  (First _)  = True
    sameCons (Second _) (Second _) = True
    sameCons _          _          = False

    isBothL ((Both _ _):_) = True
    isBothL _              = False

metricHeader :: [String]
metricHeader = ["cpy" , "ins" , "del"]

data Metric = Metric
  { cpyN :: Int
  , insN :: Int
  , delN :: Int
  }

instance Show Metric where
  show (Metric cpy ins del) = unwords . map show $ [cpy , ins , del]

instance Monoid Metric where
  mempty = Metric 0 0 0
  (Metric c1 i1 d1) `mappend` (Metric c2 i2 d2)
    = Metric (c1 + c2) (i1 + i2) (d1 + d2)

oneCpy , oneIns , oneDel :: Metric
oneCpy = Metric 1 0 0
oneIns = Metric 0 1 0
oneDel = Metric 0 0 1

whatIs :: Diff a -> Metric
whatIs (Both _ _) = oneCpy
whatIs (First _)  = oneDel
whatIs (Second _) = oneIns

count :: [Diff a] -> Metric
count = foldr mappend mempty . map whatIs
