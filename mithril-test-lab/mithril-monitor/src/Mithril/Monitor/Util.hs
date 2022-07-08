{-# LANGUAGE OverloadedStrings #-}
module Mithril.Monitor.Util where

import Control.Concurrent as Conc
import Control.Monad(unless)
import qualified Network.HTTP.Simple as Client
import Control.Exception (catch)

-- poll until a condition is met, waiting `time` microseconds between polls
pollUntil :: IO Bool -> Int -> IO ()
pollUntil poll usec =
  do  canContinue <- poll
      unless canContinue (Conc.threadDelay usec >> pollUntil poll usec)

(++/) :: String -> String -> String
a ++/ b =
  case a of
    [] -> b
    _ | last a == '/' -> a ++ b
      | otherwise -> a ++ '/':b

ping :: String -> IO Bool
ping url =
  do  req <- Client.setRequestMethod "GET" <$> Client.parseRequest url
      response <- Client.httpNoBody req
      pure $ Client.getResponseStatusCode response `elem` [200, 204]
  `catch` doFail

  where
    doFail :: Client.HttpException -> IO Bool
    doFail _ = pure False