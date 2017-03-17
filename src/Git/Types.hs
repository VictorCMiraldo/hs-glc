-- | Here we abstract away Git-specific stuff that
--   we need all over the place.
module Git.Types where

import Data.Char (toLower)

-- | Which diff algorithm we want to run?
data DiffAlgo
  = Myers
  | Minimal
  | Patience
  | Histogram
  deriving (Eq)

-- | Case insensitive read
readDiffAlgo :: String -> Maybe DiffAlgo
readDiffAlgo = match . map toLower
  where
    match :: String -> Maybe DiffAlgo
    match "myers"     = Just Myers
    match "minimal"   = Just Minimal
    match "patience"  = Just Patience
    match "histogram" = Just Histogram
    match _           = Nothing

-- Shows DiffAlgo in a way that git understands.
instance Show DiffAlgo where
  show Myers     = "--myers"
  show Minimal   = "--minimal"
  show Histogram = "--histogram"
  show Patience  = "--patience"
