{-# LANGUAGE OverloadedStrings #-}

module Sentiment (analyzeSentiment) where

import qualified Data.Text as T
import Data.List (intersect)

positiveWords :: [T.Text]
positiveWords = ["good", "happy", "excellent", "great", "positive"]

negativeWords :: [T.Text]
negativeWords = ["bad", "sad", "terrible", "poor", "negative"]

analyzeSentiment :: T.Text -> T.Text
analyzeSentiment text
    | not (null positives) && null negatives = "Positive"
    | not (null negatives) && null positives = "Negative"
    | otherwise = "Neutral"
  where
    wordsList = T.words text
    positives = wordsList `intersect` positiveWords
    negatives = wordsList `intersect` negativeWords
