module Main where

import System.Environment
import System.IO
import System.Exit

import qualified Crawler

usageStr :: [String]
usageStr = [ "hsglc mode [mode-opts]"
           , ""
           , "Available modes are:"
           , ""
           , "    crawl       Crawl a given repository"
           , ""
           , ""
           , "For mode specific help, call 'hsglc mode --help'"
           ]

main :: IO ()
main = getArgs >>= run
  where
    run ("crawl":rest) = Crawler.main rest
    run _              = hPutStrLn stderr (unlines usageStr)

    

