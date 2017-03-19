module Result where

import Crawler.DiffIdx(ClusterN , DiffIdx)

-- We are borrowing the parsers from the logParser
import Git.LogParser(parseInt , parseFloat , lexeme, P)

import Text.Parsec
import Text.Parsec.Char
import Text.Printf

type Hash = String

-- * For every log entry we will produce a bunch of results,
--   each individual result consists in:

data Result  = Result
  { _astInfo   :: InfoAST
  , _tokDIdx   :: Maybe (ClusterN , DiffIdx)
  , _isMLI     :: Bool
  } deriving Show

data InfoAST = InfoAST
  { _astClusterN     :: ClusterN
  , _astDiffIdx      :: DiffIdx
  , _astChgParent    :: String
  , _astParendDepthI :: Int
  , _astParendDepthO :: Int
  , _astLineRange    :: (Int , Int)
  } deriving Show

-- TODO: find a better way to handle these results.

showFloat :: Float -> String
showFloat = printf "%.3f"

pad :: Int -> String -> String
pad n s = take n (s ++ repeat ' ')

noData :: Int -> String
noData = flip pad (replicate 3 '-')

formatResult :: Result -> [String]
formatResult (Result ast tok isMLI)
  = [ if isMLI then "1" else "0" ]
  ++ formatAST ast
  ++ ["| "]
  ++ maybe [noData 5 , noData 5] (\(a , b) -> [pad 5 $ show a ,  pad 5 $ showFloat b]) tok

formatAST :: InfoAST -> [String]
formatAST (InfoAST cN dI con depthI depthO (li , lo))
  =  [ pad 5 $ show li , pad 5 $ show lo ]
  ++ ["| "]
  ++ [ pad 5 $ show cN , pad 5 $ showFloat dI ]
  ++ [pad 14 con , pad 3 $ show depthI , pad 3 $ show depthO]
  
-- * Parsing Results

-- |Reads all results in a string. If something fails,
--  shows the line where it failed.
parseResults :: String -> Either String [(FilePath , Hash , Result)]
parseResults = mapM parseResultLine . lines

parseResultLine :: String -> Either String (FilePath , Hash , Result)
parseResultLine = either (Left . show) Right . runParser parse1 () ""
  where
    parse1 :: P (FilePath , Hash , Result)
    parse1 = triple <$> pQuoted
                    <*> lexeme (many1 hexDigit)
                    <*> lexeme pResult

    triple a b c = (a , b , c)

    pQuoted :: P String
    pQuoted = char '"' >> try getUntilEnd
      where
        getUntilEnd
          = (char '"' >> return [])
          <|> ((:) <$> anyChar <*> getUntilEnd)

    pMLI = (((char '1' >> return True) <|> (char '0' >> return False))
             <* notFollowedBy digit) <?> "is MLI"

    word = many1 (noneOf " \t\r\n")

    pAST
      = do
        li <- lexeme parseInt
        lo <- lexeme parseInt
        lexeme (char '|')
        cN <- lexeme parseInt
        dI <- lexeme parseFloat
        cC <- lexeme word
        depI <- lexeme parseInt
        depO <- lexeme parseInt
        return (InfoAST cN dI cC depI depO (li , lo))

    pTOK = Just <$> (try $ (,) <$> parseInt <*> parseFloat)
         <|> return Nothing

    pResult :: P Result
    pResult
      = do
        mli <- lexeme pMLI
        ast <- lexeme pAST
        lexeme (char '|')
        tok <- lexeme pTOK
        return (Result ast tok mli)

{-
test :: P a -> String -> Either ParseError a
test p s = parse p "" s

testStr :: String
testStr = "\"./src/Crawler.hs\" b792b56 1 246   246   |  9     0.526 cMatch         1   1   |  4     0.796\n\"./src/Crawler.hs\" 2c33fec 0 234   234   |  3     0.368 cCase          0   0   |  3     0.256\n\"./src/Crawler.hs\" 6b855b3 1 160   173   |  1     0.333 cFieldDecl     0   0   |  2     0.500\n"
-}
