{-# LANGUAGE OverloadedStrings #-}

module StopWords (removeStopwords) where

import qualified Data.Text as T
import qualified Data.Set as S

stopwords :: S.Set T.Text
stopwords = S.fromList ["a", "an", "the", "is", "in", "on", "and", "or", "of", "to", "with"]

removeStopwords :: T.Text -> T.Text
removeStopwords text = T.unwords $ filter (`S.notMember` stopwords) (T.words text)
