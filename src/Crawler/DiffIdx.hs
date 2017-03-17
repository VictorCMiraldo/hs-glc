module Crawler.DiffIdx (getDiffIdx , ClusterN , DiffIdx) where

import Control.Monad.State
import Data.Algorithm.Diff

type DiffIdx  = Float
type ClusterN = Int

-- | Returns the number of clusters and the difference index
--   of two lists of things.
getDiffIdx :: (Eq a) => [a] -> [a] -> (ClusterN , DiffIdx)
getDiffIdx xin xout
  = let diff                 = getDiff xin xout
        Metric cp ins del cl = crunchSt True mempty diff
        insdel = fromIntegral (ins + del)
        total  = fromIntegral (ins + del + cp + cp)
      in (cl , insdel / total)

-- |Traverses the list of edit operations,
--  returns the length and the number of clusters.
--
--  The bool flag indicates whether a new copy
--  starts a new cluster. The first call should be with True.
crunchSt :: Bool -> Metric -> [Diff a] -> Metric
crunchSt newCluster m [] = m
crunchSt newCluster m (Both _ _ : ls)
  | newCluster = let m' = m `mappend` (oneCpy `mappend` oneCluster)
                  in crunchSt False m' ls
  | otherwise  = crunchSt False (m `mappend` oneCpy) ls
crunchSt _ m (l : ls) = crunchSt True (m `mappend` whatIs l) ls

data Metric = Metric Int Int Int Int
  deriving Show

instance Monoid Metric where
  mempty = Metric 0 0 0 0
  (Metric c1 i1 d1 r1) `mappend` (Metric c2 i2 d2 r2)
    = Metric (c1 + c2) (i1 + i2) (d1 + d2) (r1 + r2)

oneCpy , oneIns , oneDel , oneCluster :: Metric
oneCpy = Metric 1 0 0 0
oneIns = Metric 0 1 0 0
oneDel = Metric 0 0 1 0
oneCluster = Metric 0 0 0 1

whatIs :: Diff a -> Metric
whatIs (Both _ _) = oneCpy
whatIs (First _)  = oneDel
whatIs (Second _) = oneIns
