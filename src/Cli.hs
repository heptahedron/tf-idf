module Cli where

import Lib
import qualified Data.Text as T

getDocuments :: FilePath -> IO [Document]
getDocuments path = fmap T.pack <$> lines <$> readFile path
