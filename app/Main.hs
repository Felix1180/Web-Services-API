{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Web.Scotty
import Sentiment (analyzeSentiment)
import Network.Wai.Middleware.Cors (cors, simpleCors)
import WordCounter (wordCount, charCount)
import StopWords (removeStopwords)
import qualified Data.Text.Lazy as T
import Data.Aeson (object, (.=))

main :: IO ()
main = scotty 3000 $ do
    -- Enable CORS for all routes
    middleware simpleCors

    get "/" $ text "Welcome to Text Analysis API!"
    
    post "/analyze" $ do
        maybeInput <- formParam "text" `catch` (\ (_ :: ScottyException) -> return T.empty)
        liftIO $ putStrLn "Processing request..."
        liftIO $ print ("Received text: " ++ show maybeInput)
        if T.null maybeInput
            then json $ object ["error" .= ("Parameter 'text' not found or empty" :: String)]
            else do
                let sentiment = analyzeSentiment (T.toStrict maybeInput)
                let wc = wordCount (T.toStrict maybeInput)
                let cc = charCount (T.toStrict maybeInput)
                let noStopwords = removeStopwords (T.toStrict maybeInput)
                json $ object
                    [ "sentiment" .= sentiment
                    , "word_count" .= wc
                    , "char_count" .= cc
                    , "text_no_stopwords" .= noStopwords
                    ]
