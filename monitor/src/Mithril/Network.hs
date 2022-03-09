{-# LANGUAGE OverloadedStrings #-}
module Mithril.Network where

import Control.Concurrent.Chan(Chan)
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent as Conc
import Control.Monad(forever, void)

import qualified Network.HTTP.Simple as Client
import qualified Web.Scotty as Scotty
import Data.Aeson (ToJSON, FromJSON)
import Data.Map (Map)
import Network.Wai.Handler.Warp as Warp
import Network.HTTP.Types.Status as HTTPStatus


-------------------------------------------------------------------------------
-- Nodes


-- A Node is something that can send and recieve messages
data Node msg = Node { sendTo   :: msg -> IO ()
                     , recvFrom :: IO msg
                     }

-- make a node from Chans
mkChanNode :: Chan m -> Chan m -> Node m
mkChanNode input output =
  Node { sendTo = Chan.writeChan output
       , recvFrom = Chan.readChan input
       }


-------------------------------------------------------------------------------
-- rest infrastructure

data RoutedMessage pid m = RoutedMessage
  { rmFrom :: pid
  , rmTo :: pid
  , rmMessage :: m
  }

sendRoutedMessage :: ToJSON msg => (pid -> pid -> Maybe String) -> RoutedMessage pid msg -> IO Bool
sendRoutedMessage route msg =
  case route (rmFrom msg) (rmTo msg) of
    Nothing -> pure False
    Just url -> sendMsg url (rmMessage msg)

sendMsg :: ToJSON msg => String -> msg -> IO Bool
sendMsg url msg =
  do  req <- Client.setRequestMethod "POST" .
             Client.setRequestBodyJSON msg <$> Client.parseRequest url

      response <- Client.httpNoBody req
      pure $ Client.getResponseStatusCode response == 200

routedMessageEndpoint :: (Scotty.Parsable pid, FromJSON msg) => (RoutedMessage pid msg -> IO ()) -> Scotty.ScottyM ()
routedMessageEndpoint o =
  do  Scotty.post "/:to/:from" $
        do  to <- Scotty.param "to"
            from <- Scotty.param "from"
            msg <- Scotty.jsonData

            let rmsg = RoutedMessage from to msg
            Scotty.liftAndCatchIO (o rmsg)

      Scotty.defaultHandler $ \_ -> Scotty.status HTTPStatus.status400

restNode :: (Scotty.Parsable pid, FromJSON msg, ToJSON msg) => Warp.Settings -> (pid -> pid -> Maybe String) -> IO (Node (RoutedMessage pid msg))
restNode settings route =
  do  inChan <- Chan.newChan
      outChan <- Chan.newChan

      let ep = routedMessageEndpoint (Chan.writeChan inChan)
      -- TODO: don't ignore errors?
      Conc.forkIO $ forever (Chan.readChan outChan >>= sendRoutedMessage route)

      let sopts = Scotty.Options  { Scotty.verbose = 0, Scotty.settings = settings }
      Conc.forkIO $ Scotty.scottyOpts sopts ep

      pure $ mkChanNode inChan outChan





