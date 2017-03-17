module GitLog where

import Control.Arrow((***))
import Data.Char(isDigit)
import Data.List(break, groupBy)

import Control.Monad.State

-- * Library functions

takeDropWhile :: (a -> Bool) -> [a] -> ([a] , [a])
takeDropWhile p [] = ([] , [])
takeDropWhile p (x:xs)
  | not (p x) = ([] , x:xs)
  | otherwise = let (a , b) = takeDropWhile p xs
                 in (x:a , b)

extract :: String -> String -> Maybe String
extract _ []  = Just []
extract [] ss = Just ss 
extract (x:xs) (s:ss)
  | x == s    = extract xs ss
  | otherwise = Nothing

dropSuffix :: String -> String -> Maybe String
dropSuffix _ []     = Nothing
dropSuffix s (x:xs) = maybe ((x:) <$> dropSuffix s xs) (const (Just []))
                            (extract s (x:xs))

between :: String -> String -> Maybe String
between s ss = extract s ss >>= dropSuffix s

readNumbers :: String -> [Int]
readNumbers [] = []
readNumbers s  = (uncurry (:)) . (read *** (readNumbers . dropNN)) . nums $ s
  where
    dropNN = dropWhile (not . isDigit)

    nums :: String -> (String , String)
    nums = takeDropWhile isDigit . dropNN

-- Reads the start line from both original file and changed file
-- for instance;
--
--  readLineInfo "@@ +142,23 -150,26 @@ some more text"
--     == (142,150)
--
-- WARNING: pretty fragile!
readLineInfo :: String -> (Int , Int)
readLineInfo s = case readNumbers <$> between "@@" s of
                   Just [l1i , l1s , l2i , l2s] -> (l1i , l2i)
                   Just [l1i , l1s , l2i] -> (l1i , l2i)
                   Just [l1i , l2i] -> (l1i , l2i)
                   x -> error ("lol" ++ show x ++ "\n" ++ show s)

-- Here we parse the output of
-- git log --pretty=format:"#%n%h" --follow -p --no-color -- <file.hs>

data GitChange
  = GitChange { _lineSrc   :: Int
              , _lineDst   :: Int
              , _ins       :: [String]
              , _del       :: [String]
              }
    deriving Show

data GitLogChg
  = GitLogChg { _commit   :: String
              , _fin      :: String
              , _fout     :: String
              , _changes  :: [GitChange]
              }
    deriving Show

-- A GitChange is really a change when there are insertions and
-- deletions.
isChange :: GitChange -> Bool
isChange (GitChange _ _ (_:_) (_:_)) = True
isChange _                           = False

-- Parse the log as a single string.
parseLog :: String -> [GitLogChg]
parseLog = parseLines . lines

-- Parse individual lines. We made sure a patch starts with a '#'
parseLines :: [String] -> [GitLogChg]
parseLines ("#":lines) = let (this , rest) = break (== "#") lines
                             rec = parseLines rest
                          in parseChangeSet this : parseLines rest
parseLines _ = []                             

-- Parse a single change set
parseChangeSet :: [String] -> GitLogChg
parseChangeSet (hash:_:_:fin:fout:juice)
  = GitLogChg hash (parseFile fin) (parseFile fout) (parseChanges juice)
  where
    parseFile = drop 6
parse1 _ = []

isIns :: String -> Bool
isIns ('+':_) = True
isIns _       = False

isDel :: String -> Bool
isDel ('-':_) = True
isDel _       = False

isChg :: String -> Bool
isChg ('@':'@':_) = True
isChg _           = False


-- Parses individual changes. Returns only blocks of change that has
-- a sequence of insertions followed by a sequence of deletions;
-- that is, a modify.
parseChanges :: [String] -> [GitChange]
parseChanges []         = []
parseChanges (cInfo:xs)
  | isChg cInfo
    = let (this , rest) = break isChg xs
          li            = readLineInfo cInfo
          -- li'           = hasLineIn li cInfo
       in getChanges li this ++ parseChanges rest
  | otherwise = parseChanges xs
  where
    hasLineIn :: (Int , Int) -> String -> (Int , Int)
    hasLineIn (ls , ld) ('@':'@':s)
      = case dropWhile (/= '@') s of
            ('@':'@':x:xs) -> (ls + 1 , ld + 1)
            _            -> (ls , ld)

   

    isCopy :: [String] -> Bool
    isCopy [] = True
    isCopy (x:_) = x == [] || head x == ' '

    isInsDel :: String -> Bool
    isInsDel x = isIns x || isDel x

    -- The groupBy will separate lines that are copied from lines
    -- that are inserted/deleted.
    getChanges :: (Int , Int) -> [String] -> [GitChange]
    getChanges li = getGroupedChanges li
                  . groupBy (\a b -> isInsDel a && isInsDel b)

    getGroupedChanges :: (Int , Int) -> [[String]] -> [GitChange]
    getGroupedChanges (ls , ld) [] = []
    getGroupedChanges (ls , ld) (s:ss)
      -- If "s" is a copied line, step both counters
      | isCopy s = getGroupedChanges (ls + 1 , ld + 1) ss
      -- Otherwise, "s" is a list of insertions and deletions.
      | otherwise
      = let insS = map tail $ filter isIns s
            delS = map tail $ filter isDel s
         in (GitChange ls ld insS delS)
            : getGroupedChanges (ls + length delS , ld + length insS) ss


pretty :: [GitLogChg] -> IO ()
pretty = mapM_ pretty1
  where
    pretty1 :: GitLogChg -> IO ()
    pretty1 (GitLogChg hash fin fout chgs)
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


test :: FilePath -> IO [GitLogChg]
test s = do r <- readFile s
            return (parseLog r)

