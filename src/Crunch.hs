{-# LANGUAGE TemplateHaskell #-}
module Crunch where

import qualified Data.Map as M
import Data.List (sortBy)
import Data.Function (on)

import Text.Printf

import Crunch.Options
import Result

data TmpData
  = TD { _tmpTotalIdx :: Float
       , _tmpMLIcount :: Int
       , _tmpSample   :: Int
       , _tmpDropped  :: Int
       , _tmpCtrMap   :: M.Map String Int
       } deriving Show

emptyData :: TmpData
emptyData = TD 0 0 0 0 M.empty
    
main :: [String] -> IO ()
main args
  = do
    (opts , files) <- compileOpts args
    ctes           <- concat <$> mapM readFile files
    case parseResults ctes of
      Left err -> ioError (userError ("No parse: err"))
      Right re -> outputData (crunchResults opts (map third re))
  where
    third (_ , _, x) = x

    divPrint :: Int -> Int -> String
    divPrint x y
      = printf "%.3f" ((fromIntegral x / fromIntegral y) :: Float)

    outputData :: TmpData -> IO ()
    outputData (TD idxT mliC tot dr m)
      = do
        putStrLn $ "I looked at " ++ show tot
        putStrLn $ "  (dropped " ++ show dr ++
                   "; " ++ divPrint dr (dr + tot) ++ ")"
        putStrLn $ ""
        putStrLn $ "mli's are: " ++ divPrint mliC tot
        putStrLn $ ""
        putStrLn $ "Top constructors are: "
        prettyMap (M.map (\x -> fromIntegral x / fromIntegral tot) m)

    prettyMap :: M.Map String Float -> IO ()
    prettyMap m = let l = sortBy (flip compare `on` snd) $ M.toList m
                      l' = filter ((>= 0.25) . snd) l
                   in mapM_ (uncurry pretty1) l'

    pretty1 cons f
      = putStrLn $ pad 20 cons ++ printf "%.4f" f
        

crunchResults :: Opts -> [Result] -> TmpData
crunchResults opts r = crunchAux opts r emptyData

crunchAux :: Opts -> [Result] -> TmpData -> TmpData
crunchAux opts [] d = d
crunchAux opts (r:rs) d
  | eligible opts r = crunchAux opts rs (update opts d r)
  | otherwise       = crunchAux opts rs
                           (d { _tmpDropped = _tmpDropped d + 1})
  where
    eligible :: Opts -> Result -> Bool
    eligible (Opts minIdx maxIdx False) r
      = let dI = _astDiffIdx (_astInfo r)
         in minIdx <= dI && dI <= maxIdx
    eligible (Opts minIdx maxIdx True) r
      = case _tokDIdx r of
           Nothing       -> False
           Just (_ , dI) -> minIdx <= dI && dI <= maxIdx      

    update :: Opts -> TmpData -> Result -> TmpData
    update opts d r
      = let idx = if (optUseTok opts)
                  then maybe 0 snd $ _tokDIdx r
                  else _astDiffIdx (_astInfo r)
            ctr = _astChgParent $ _astInfo r
         in TD (idx + _tmpTotalIdx d)
               (_tmpMLIcount d + if _isMLI r then 1 else 0)
               (_tmpSample d + 1)
               (_tmpDropped d)
               (M.alter countCtr ctr (_tmpCtrMap d))
      where
        countCtr = maybe (Just 1) (Just . (+1))


-- Given a mean c for t values, compute the mean for
-- adding idx in the sample set.
updateMean :: Float -> Int -> Float -> Float
updateMean idx 0 c = idx
updateMean idx t c = c + (1.0 / fromIntegral t) * (idx - c)


testData :: [Result]
testData = map (\(_ , _ , x) -> x)  [("./src/Crawler.hs","b792b56",Result {_astInfo = InfoAST {_astClusterN = 9, _astDiffIdx = 0.526, _astChgParent = "cMatch", _astParendDepthI = 1, _astParendDepthO = 1, _astLineRange = (246,246)}, _tokDIdx = Just (4,0.796), _isMLI = True}),("./src/Crawler.hs","2c33fec",Result {_astInfo = InfoAST {_astClusterN = 3, _astDiffIdx = 0.368, _astChgParent = "cCase", _astParendDepthI = 0, _astParendDepthO = 0, _astLineRange = (234,234)}, _tokDIdx = Just (3,0.256), _isMLI = False}),("./src/Crawler.hs","6b855b3",Result {_astInfo = InfoAST {_astClusterN = 1, _astDiffIdx = 0.333, _astChgParent = "cFieldDecl", _astParendDepthI = 0, _astParendDepthO = 0, _astLineRange = (160,173)}, _tokDIdx = Just (2,0.5), _isMLI = True})]

