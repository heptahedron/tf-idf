module Lib where

import Data.Char (isAlphaNum)
import qualified Data.Text as T
import Data.Text.ICU.Normalize
import qualified Data.Map as M
import qualified Data.List as L

type Document = T.Text
type Term = T.Text
type Freq = Double
type DVec = M.Map Term Freq
type Relevance = Double

-- | Returns a list of terms from the document, in the order
-- | and quantity of appearance, case-folded
termSeq :: Document -> [Term]
termSeq = map (normalize FCD . T.toCaseFold)
          . filter (not . T.null)
          . T.split (not . isAlphaNum)

-- | Returns a map from terms to the number of their occurrences
termCounts :: [Term] -> DVec
termCounts = foldr (flip (M.insertWith (+)) 1) M.empty

-- | Returns a map from terms to their frequencies
termFreqs :: [Term] -> DVec
termFreqs ts = (/ (fromIntegral $ length ts)) <$> termCounts ts

compareFreqs :: DVec -> DVec -> Relevance
compareFreqs = (foldr (+) 0 .) . M.unionWith ((abs .) . (-))

compareDocs a b = compareFreqs (termFreqs $ termSeq a) (termFreqs $ termSeq b)

rankRelevance :: (Document -> Document -> Relevance)
              -> Document
              -> [Document]
              -> [(Int, Document)]
rankRelevance rel query
  = map (\((i, d), r) -> (r, d))
  . L.sortOn (fst . fst)
  . flip zip [0..]
  . L.sortOn (rel query . snd)
  . zip [0..]

pluck :: Int -> [a] -> Maybe (a, [a])
pluck i xx = case splitAt i xx of
  (xs, a:xs') -> Just (a, xs ++ xs')
  _           -> Nothing
