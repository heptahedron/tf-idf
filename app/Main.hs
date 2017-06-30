module Main where

import Lib
import Cli
import System.Environment

main :: IO ()
main = do
  [docsFile, iStr] <- getArgs
  let i = read iStr :: Int
  docs <- getDocuments docsFile
  case pluck i docs of
    Just (queryDoc, corpus) ->
      mapM_ print $ map fst $ rankRelevance compareDocs queryDoc corpus
    _ -> print "Invalid document index."
