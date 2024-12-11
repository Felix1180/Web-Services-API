module Utils where

import Data.Char (toLower)
import qualified Data.Text as T

normalizeText :: T.Text -> T.Text
normalizeText = T.toLower . T.strip
