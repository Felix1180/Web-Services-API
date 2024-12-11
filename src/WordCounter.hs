{-# LANGUAGE OverloadedStrings #-}

module WordCounter (wordCount, charCount) where

import qualified Data.Text as T

wordCount :: T.Text -> Int
wordCount = length . T.words

charCount :: T.Text -> Int
charCount = T.length
