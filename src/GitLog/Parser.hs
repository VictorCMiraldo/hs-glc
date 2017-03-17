module GitLog.Parser where

import Text.Parsec
import Text.Parsec.Char

import Control.Arrow((***))
import Data.Char(isDigit)
import Data.List(break, groupBy)

import Control.Monad.State

data UnifiedDiff = UD
  { _srcLine :: Int
  , _srcSpan :: Int
  , _dstLine :: Int
  , _dstSpan :: Int
  }

-- | Shows it nicely already!
--   @@ -16,7 +16,8 @@
instance Show UnifiedDiff where
  show (UD sl ss dl ds)
    = unwords ["@@"
              , concat [ "-" , show sl , "," , show ss ]
              , concat [ "+" , show dl , "," , show ds ]
              , "@@"
              ]

-- Here we parse the output of
-- git log --pretty=format:"#%n%h" --follow -p --no-color -- <file.hs>

data GitChange
  = GitChange { _lineSrc   :: Int
              , _lineDst   :: Int
              , _ins       :: [String]
              , _del       :: [String]
              }
    deriving Show

data GitLogEntry
  = GitLogEntry { _commit   :: String
              , _fin      :: String
              , _fout     :: String
              , _changes  :: [GitChange]
              }
    deriving Show

-- | Our parser runs over strings, we keep no user-state.
type P = Parsec String ()

-- |Parses an Int number
parseInt :: P Int
parseInt = read <$> many1 digit

-- | Runs p on the lines of our input.
parseLines :: P a -> P [a]
parseLines p = p `sepEndBy1` endOfLine

-- | Gets the rest of line, consumes endOfLine
restOfLine :: P String
restOfLine = many (noneOf "\r\n") <* endOfLine

-- | Gets the rest of line, consumes endOfLine or eof
restEOfLine :: P String
restEOfLine = many (noneOf "\r\n") <* ((endOfLine >> return ()) <|> eof)

-- | Skip a line
skipLine :: P ()
skipLine = restOfLine >> return ()

-- | Skip whitespaces before parsing p
lexeme :: P a -> P a
lexeme p = spaces >> p

-- | Parses a unified diff '@@ -16,7 +16,8 @@'
parseUnifiedDiff :: P UnifiedDiff
parseUnifiedDiff
  = (string "@@"
  *> (UD <$> lexeme (char '-' >> parseInt)
         <*> (char ',' >> parseInt)
         <*> lexeme (char '+' >> parseInt)
         <*> (char ',' >> parseInt)) 
  <* lexeme (string "@@")) <?> "Unified Diff Info"
  
-- |This is now specific for us:
parseGitLogEntry :: P GitLogEntry
parseGitLogEntry = char '#' >> endOfLine >>
    (GitLogEntry <$> ((pHash  <?> "Commit hash") <* skipLine <* skipLine)
                 <*> (pSrcFile <?> "Source File Info")
                 <*> (pDstFile <?> "Dest File Info")
                 <*> parseGitChanges) <?> "Commit Information"
  where
    pHash    = many hexDigit <* endOfLine
    pSrcFile = string "---" >> lexeme (string "a/" >> restOfLine)
    pDstFile = string "+++" >> lexeme (string "b/" >> restOfLine)

parseGitChanges :: P [GitChange]
parseGitChanges = (concat <$> many parseGitChange1)

parseGitChange1 :: P [GitChange]
parseGitChange1
  = do
    ud    <- parseUnifiedDiff <* restOfLine
    pChanges (_srcLine ud, _dstLine ud)
  where
    -- Now we keep a counter of lines
    -- and return all the changes we see
    pChanges :: (Int , Int) -> P [GitChange]
    pChanges (src , dst)
      =   ((char ' ' >> skipLine >> pChanges (src + 1 , dst + 1))
      <|> (pBlock >>= \(ins , del) -> ((GitChange src dst ins del) :)
                                  <$> pChanges (src + length del , dst + length ins))
      <|> (return [])) <?> "here"

    pBlock = (pPlusBlock <|> pMinusBlock) <?> "Change Block"

    pPlusBlock :: P ([String] , [String])
    pPlusBlock
      = do
        plus  <- many1 (char '+' >> restEOfLine)
        minus <- many  (char '-' >> restEOfLine)
        return (plus , minus)

    pMinusBlock :: P ([String] , [String])
    pMinusBlock
      = do
        minus <- many1 (char '-' >> restEOfLine)
        plus  <- many  (char '+' >> restEOfLine)
        return (plus , minus)

parseGitLog :: P [GitLogEntry]
parseGitLog = many parseGitLogEntry

-- | Parse 'git log' result. First parameter is the description
--   of the log.
parseGitLogEntries :: String -> String -> Either ParseError [GitLogEntry]
parseGitLogEntries desc c = runParser parseGitLog () desc c
         

pretty :: [GitLogEntry] -> IO ()
pretty = mapM_ pretty1
  where
    pretty1 :: GitLogEntry -> IO ()
    pretty1 (GitLogEntry hash fin fout chgs)
      =  putStrLn "#######"
      >> putStrLn hash
      >> putStrLn ("--- a/" ++ fin)
      >> putStrLn ("+++ b/" ++ fout)
      >> mapM_ prettyChgs chgs

    prettyChgs :: GitChange -> IO ()
    prettyChgs (GitChange ls ld insS delS)
      =  putStrLn ("@@ -" ++ show ls ++ " , +" ++ show ld ++ " @@")
      >> mapM_ (putStrLn . ('+':)) insS
      >> mapM_ (putStrLn . ('-':)) delS


test :: FilePath -> IO ()
test s = do c <- readFile s
            case parseGitLogEntries "" c of
              Left err -> putStrLn (show err)
              Right c  -> pretty c
