module Crawler(main) where

import Control.Monad.Reader
import Control.Monad.Except
import Text.Printf
import System.IO

import HsInfo.ASTInfo
import Git.LogParser
import Git.Process
import Git.Types
import Errors

import Crawler.Options
import Crawler.DiffIdx

-- * Library functions

showFloat :: Float -> String
showFloat = printf "%.3f"

-- Crawler monad; encapsulates program options and
-- custom exceptions on top of the IO monad.
type CrawlerM = ReaderT Opts ErrIO

runCrawlerM :: CrawlerM x -> Opts -> IO x
runCrawlerM crl o = runErrIO $ runReaderT crl o

getOpt :: (Opts -> a) -> CrawlerM a
getOpt f = ask >>= return . f

whenOpt :: (Opts -> Bool) -> CrawlerM () -> CrawlerM ()
whenOpt f act = getOpt f >>= \x -> if x then act else return ()

whenVerb :: String -> CrawlerM ()
whenVerb msg
  = do
    isVerb <- getOpt optVerb
    liftIO (when isVerb $ hPutStrLn stderr msg)

-- Entry point for the log crawler.
main :: [String] -> IO ()
main args
  = do
    opts <- compileOpts args
    runCrawlerM crawl opts

-- Runs and parses the 'git log' for the input we want.
crawl_runGitLog :: CrawlerM [GitLogEntry]
crawl_runGitLog
  = do
    algo  <- getOpt optDiffAlgo
    input <- getOpt optInput
    str   <- runGit GitLogFail (liftIO $ runGitLogWithAlgo algo input)
    case parseGitLogEntries input str of
      Left err -> throwError $ GitLogParse input (show err)
      Right r  -> return r

-- Runs and parses the 'git log' for the input we want.
crawl_runGitShow :: String -> CrawlerM HsModule
crawl_runGitShow s
  = do
    str   <- runGit GitShowFail (liftIO $ runGitShow s)
    case hsParseModule str of
      Left err -> throwError $ GitShowParse s (show err)
      Right r  -> return r

crawl :: CrawlerM ()
crawl
  = do
    -- XXX: implement
    whenOpt optHeader (throwError NotImplemented)

    -- Crawling the log is trivial now;
    -- First we parse the log,
    gitlog <- crawl_runGitLog

    -- Then we process each entry individually.
    mapM_ processEntry gitlog

-- |Process a single GitLogEntry
--  If we fail to parse the module, we abort this entry.
processEntry :: GitLogEntry -> CrawlerM ()
processEntry (GitLogEntry hash fin fout chg)
  = do i <- quote <$> getOpt optInput
       (parsePrePos >>= processAll >>= mapM_ (maybe (return ()) (outputResult i)))
      `catchError` catch
  where
    outputResult :: String -> Result -> CrawlerM ()
    outputResult qfname r
      = liftIO . putStrLn . unwords $ [ qfname , hash ] ++ formatResult r

    quote s = '"':s ++ ['"']

    -- We catch the errors that could happen
    -- when running parsePrePos
    catch err@(GitShowFail  ex msg)
      = whenVerb ("dropping entry:" ++ hash ++ "\n\t" ++ show err)
    catch err@(GitShowParse ex msg)
      = whenVerb ("dropping entry:" ++ hash ++ "\n\t" ++ show err)
    catch err
      = throwError err

    parsePrePos :: CrawlerM (HsModule , HsModule)
    parsePrePos
      = do
        let posFileName = hash ++  ":" ++ fout
        let preFileName = hash ++ "^:" ++ fin
        preM <- crawl_runGitShow preFileName
        posM <- crawl_runGitShow posFileName
        return (preM , posM)
  
    processAll :: (HsModule , HsModule) -> CrawlerM [Maybe Result]
    processAll mod = mapM (processChange mod) chg

    isMLI :: GitChange -> Bool
    isMLI c = length (_ins c) == length (_del c)

    -- Something that only inserts or deletes is not a change!
    isNotChange :: GitChange -> Bool
    isNotChange (GitChange _ _ [] _) = True
    isNotChange (GitChange _ _ _ []) = True
    isNotChange (GitChange _ _ _ _) = False

    processChange :: (HsModule , HsModule) -> GitChange -> CrawlerM (Maybe Result)
    processChange ms chg
      = do
        keepNoChg <- getOpt optKeepNoChg
        ast       <- getASTInfo ms chg
        if not keepNoChg && isNotChange chg 
        then return Nothing
        else Just <$> (Result ast <$> getTokenDiffIdx chg <*> return (isMLI chg))
        
-- * For every log entry we will produce a bunch of results,
--   each individual result consists in:

data Result  = Result
  { _astInfo   :: InfoAST
  , _tokDIdx   :: Maybe (ClusterN , DiffIdx)
  , _isMLI     :: Bool
  } deriving Show

data InfoAST = InfoAST
  { _astClusterN  :: ClusterN
  , _astDiffIdx   :: DiffIdx
  , _astConChange :: Maybe (String , String)
  , _astLineRange :: (Int , Int)
  } deriving Show

-- TODO: find a better way to handle these results.
pad :: Int -> String -> String
pad n s = take n (s ++ repeat ' ')

noData :: Int -> String
noData = flip pad (replicate 3 '-')

formatResult :: Result -> [String]
formatResult (Result ast tok isMLI)
  = [ if isMLI then "1" else "0" ]
  ++ formatAST ast
  ++ maybe [noData 5 , noData 5] (\(a , b) -> [pad 5 $ show a ,  pad 5 $ showFloat b]) tok

formatAST :: InfoAST -> [String]
formatAST (InfoAST cN dI cn (li , lo))
  = [ pad 5 $ show cN , pad 5 $ showFloat dI ]
  ++ maybe [noData 10, noData 10] (\(a , b) -> [pad 10 a , pad 10 b]) cn
  ++ [ pad 5 $ show li , pad 5 $ show lo ]

-- * Producing the results:

-- |Get the results for the LCS on the token level.
getTokenDiffIdx :: GitChange -> CrawlerM (Maybe (ClusterN , DiffIdx))
getTokenDiffIdx (GitChange lSrc lDst ins del)
  = case hsTokenize ins del of
      Left  err       -> whenVerb ("Tokenizer failed: " ++ err)
                      >> return Nothing
      Right (tI , tD) -> return . Just $ getDiffIdx tI tD

-- * Information we gather on the AST level

-- Returns the constructors that changed in the AST
-- and the Difference index of the LCS at the AST level.
getASTInfo :: (HsModule , HsModule) -> GitChange -> CrawlerM InfoAST
getASTInfo (modPre , modPos) (GitChange lSrc lDst ins del)
  = do
    -- Are we using columns to fine-tune the ast?
    useCols <- getOpt optColBased
    let col = if useCols then getCol ins del else Nothing

    -- Make the line-range we need.
    let ls  = LR lSrc col lDst

    -- Get the parent of the constructor that changed
    -- in both the source and the destination.
    let conInfo = (,) <$> firstConParentOf ls modPre
                      <*> firstConParentOf ls modPos

    -- Get the constructors of the regions of the AST
    let cPre = allConsIn ls modPre
    let cPos = allConsIn ls modPos
    let (cl , didx) = getDiffIdx cPre cPos
    return $ InfoAST cl didx conInfo (lSrc , lDst)
  where

    -- gets the first column in which things differ.
    getCol [] [] = Nothing
    getCol (x:_) (y:_)
      = Just $ length (takeWhile (uncurry (==)) (zip x y))
