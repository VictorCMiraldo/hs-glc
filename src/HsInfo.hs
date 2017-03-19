module HsInfo(main) where


-- This module is used for debugging the
-- retrieval of constructor infromation.

import System.IO
import System.Process

import Text.Parsec
import Text.Parsec.Char (digit)
import HsInfo.ASTInfo


-- * Usage Str

showUsage :: IO ()
showUsage = mapM_ putStrLn usageStr
  where
    usageStr = ["hsglc info m[,c]:n <revision:./path/to/file>"]

-- * Parse a LR

parseLR :: String -> Maybe LineRegion
parseLR = either (const Nothing) Just . runParser pLR () "<>"
  where    
    pLR = LR <$> (read <$> many1 digit)
             <*> (option Nothing (Just . read
                                  <$> (char ',' >> many1 digit)))
             <*> (read <$> (char ':' >> many1 digit))

main :: [String] -> IO ()
main [pos , file]
  | Just lr <- parseLR pos
  = hsinfo lr file
main _ = showUsage

hsinfo :: LineRegion -> String -> IO ()
hsinfo lr file
  = do
    out <- readProcess "git" ["show" , file] ""
    case hsParseModule out of
      Left str -> putStrLn $ "error: " ++ str
      Right m  -> printRange lr (lines out)
               >> putStrLn "==="
               >> getInfo lr m
  where    
    getInfo lr m
      = let st = stackIn lr m
         in mapM_ formatOut st >> putStrLn ""

    formatOut (c , t)
      = putStr $ showDef "_" c ++ " :: " ++ showDef "_" t ++ " | "

    showDef s "" = s
    showDef _ s  = s


printRange :: LineRegion -> [String] -> IO ()
printRange (LR l c m) file
  = let interval = pad c $ take (m - (l - 1)) $ drop (l - 1) file
     in mapM_ putStrLn interval
  where
    pad Nothing s  = s
    pad (Just c) (s:ss)
      = (replicate (c - 1) '.' ++ drop (c -1) s) : ss
