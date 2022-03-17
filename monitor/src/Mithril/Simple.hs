{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Mithril.Simple where

import qualified Data.List as List
import Data.Text(Text)
import qualified Data.Text.IO as TIO
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Maybe(maybeToList, isNothing)
import Data.Foldable(foldl')
import qualified System.Process as Process
import qualified Data.Aeson as JSON
import Data.Aeson((.=))
import qualified System.Directory as Dir
import qualified System.IO.Temp as Temp
import Control.Monad(forM_, forever, when)
import qualified Data.IORef as IORef
import Control.Exception(bracket)
import qualified Control.Concurrent as Conc

import qualified Mithril.Network as MNet
import qualified Mithril.Monitor as Monitor
import qualified Mithril.Messages as Messages
import Mithril.Messages (PartyId, Stake)
import Mithril.Util(pollUntil, (++/), ping)
import qualified Mithril.Messages as Message

import qualified Debug.Trace as Trace


type RMsg = MNet.RoutedMessage Messages.PartyId Messages.Message

data MOutput =
    Send RMsg
  | Err String

type Monitor = Monitor.Monitor RMsg MOutput

send :: RMsg -> Monitor ()
send = Monitor.output . Send

sendFromTo :: PartyId -> PartyId -> Messages.Message -> Monitor ()
sendFromTo from to msg = send $ MNet.RoutedMessage from to msg

err :: String -> Monitor ()
err = Monitor.output . Err

-- configure a lab and run a monitor after setting up
testMonitor :: LabConfig -> Monitor () -> Bool -> IO ()
testMonitor lc mon log =
  do  withLab lc (`go` mon)

  where
    go labNode mon =
      do  -- read any monitor output
          let (mon', out) = Monitor.getOutput mon

          -- dispatch all output produced by the monitor
          out `forM_` \case
            Send msg -> MNet.sendTo labNode msg
            Err e -> putStrLn e

          -- if monitor is complete - finish the computation
          case Monitor.getResult mon' of
            Nothing ->
              do  msg <- MNet.recvFrom labNode
                  MNet.sendTo labNode msg
                  when log (printMsg msg)
                  go labNode (mon' `Monitor.observe` msg)

            Just () -> pure ()



defaultParams :: Messages.Parameters
defaultParams =
  Messages.Parameters { Messages.parametersK = 5
                      , Messages.parametersM = 100
                      , Messages.parametersPhiF = 0.2
                      }



-------------------------------------------------------------------------------
-- Config/Setup

data NodeConfig = NodeConfig
  { nConfigPartyId :: PartyId
  , nConfigLocalAddress :: Maybe String
  , nConfigExtAddress :: String
  , nConfigStake :: Stake
  }
  deriving(Eq)

data NetworkConfig = NetworkConfig
  { netNodes :: [NodeConfig]
  , netParams :: Messages.Parameters
  }
  deriving(Eq)

data LabConfig = LabConfig
  { labNet :: NetworkConfig
  , labRestPort :: Int
  , labRustDir :: String
  }

labRestBase :: LabConfig -> String
labRestBase lc = MNet.restAddress (labRestPort lc)

instance JSON.ToJSON NodeConfig where
  toJSON nc =
    JSON.object [ "id" .= nConfigPartyId nc
                , "address_local" .= nConfigLocalAddress nc
                , "address_endpoint" .= nConfigExtAddress nc
                , "stake" .= nConfigStake nc
                ]

instance JSON.ToJSON NetworkConfig where
  toJSON nc =
    JSON.object [ "nodes" .= netNodes nc
                , "parameters" .= netParams nc
                ]

localConfig :: PartyId -> Stake -> Int -> NodeConfig
localConfig pid stake port =
  NodeConfig { nConfigPartyId = pid
             , nConfigLocalAddress = Just ("localhost:" ++ show port)
             , nConfigExtAddress = "http://localhost:" ++ show port ++ "/"
             , nConfigStake = stake
             }

virtualConfig :: PartyId -> Stake -> NodeConfig
virtualConfig pid stake =
  NodeConfig { nConfigPartyId = pid
             , nConfigLocalAddress = Nothing
             , nConfigExtAddress = ""
             , nConfigStake = stake
             }

isVirtualNode :: NodeConfig -> Bool
isVirtualNode c = isNothing (nConfigLocalAddress c)

proxyNodeConfig :: String -> NodeConfig -> NodeConfig
proxyNodeConfig baseUri config =
  config { nConfigExtAddress = baseUri ++ show (nConfigPartyId config) ++ "/"
         }

proxyNetConfigFor :: (NodeConfig -> NodeConfig) -> NodeConfig -> NetworkConfig -> NetworkConfig
proxyNetConfigFor pxy node net = net { netNodes = proxy <$> netNodes net }
  where
    proxy n | n == node = node
            | otherwise = pxy n

withLaunchNode :: LabConfig -> NodeConfig -> IO a -> IO a
withLaunchNode labCfg nc a =
  case nConfigLocalAddress nc of
    Nothing -> a
    Just la ->
      withNetCfgFile proxiedNetConfig $ \netCfgFile ->
        let pid = nConfigPartyId nc
            nproc = Process.proc "cargo" ["run", "--", "--node-id", show pid, "--config-file", netCfgFile]
            nproc' = nproc { Process.cwd = Just (labRustDir labCfg) }
        in Process.withCreateProcess nproc' $ \_ _ _ _ ->
          do  pollUntil  (ping (nConfigExtAddress nc ++/ "ping")) (200 * 1000)
              a

  where
    proxiedNetConfig = proxyNetConfigFor (proxyNodeConfig (labRestBase labCfg)) nc (labNet labCfg)



route :: NetworkConfig -> PartyId -> PartyId -> Maybe String
route nc from to = (++/ show from) . nConfigExtAddress <$> nodeConfig
  where
    nodeConfig = List.find (\c -> nConfigPartyId c == to && not (isVirtualNode c)) (netNodes nc)

withNetCfgFile :: NetworkConfig -> (FilePath -> IO a) -> IO a
withNetCfgFile nc f =
  --Temp.withSystemTempFile "testlab-netconfig" (\path _ -> JSON.encodeFile path nc >> f path)
  bracket (Temp.emptySystemTempFile "testlab-netconfig")
          Dir.removeFile
          (\path -> JSON.encodeFile path nc >> f path)

withLab :: LabConfig -> (MNet.Node RMsg -> IO a) -> IO a
withLab labCfg a =  MNet.withRestNode (labRestPort labCfg) (route netCfg) (withLaunchNodes labCfg . a)
  where
    netCfg = labNet labCfg
    withLaunchNodes labCfg a = foldr (withLaunchNode labCfg) a (netNodes netCfg)

-------------------------------------------------------------------------------

labConfig1 :: LabConfig
labConfig1 =
  LabConfig { labNet = labNetCfg
            , labRustDir = "../rust-node"
            , labRestPort = 8000
            }
  where
    labNetCfg =
      NetworkConfig { netParams = defaultParams
                    , netNodes = [ localConfig 1 1 8001
                                 , localConfig 2 1 8002
                                 ]
                    }

printMsg :: RMsg -> IO ()
printMsg msg =
  do  putStrLn (show (MNet.rmFrom msg) ++ " --> " ++ show (MNet.rmTo msg))
      print (MNet.rmMessage msg)
      putStrLn ""

logMessagesTest :: IO ()
logMessagesTest =
  do  withLab labConfig1 $ \lc ->
        do  tid <- Conc.forkIO . forever $
                    do  msg <- MNet.recvFrom lc
                        printMsg msg
                        MNet.sendTo lc msg

            _ <- getLine
            Conc.killThread tid
            pure ()


testMonitorInit :: Bool -> IO ()
testMonitorInit = testMonitor labConfig1 monitorInit


messagePayload :: RMsg -> Messages.Payload
messagePayload = Messages.messagePayload . MNet.rmMessage

messageFrom :: RMsg -> Messages.PartyId
messageFrom = MNet.rmFrom

messageTo :: RMsg -> Messages.PartyId
messageTo = MNet.rmTo

monitorInit :: Monitor ()
monitorInit = go Map.empty
  where
    go :: Map Messages.PartyId Messages.Hello -> Monitor ()
    go map =
      Monitor.on $ \m ->
        case messagePayload m of
          Messages.PayloadHello h ->
            case Map.lookup (Messages.helloPartyId h) map of
              Nothing ->
                do  let map' =  Map.insert (Messages.helloPartyId h) h map
                        target = messageTo m
                    Monitor.allInParallel_ [go map', monitorHelloCompletesFor target h]

              Just _ -> go map
          Messages.PayloadInitComplete _ -> pure ()
          _ -> go map

monitorHelloCompletesFor :: Messages.PartyId -> Messages.Hello -> Monitor ()
monitorHelloCompletesFor p hello =
  Monitor.filterInput ((==p) . messageFrom) $
    do  parties <- Monitor.onJust $ \m ->
          case messagePayload m of
            Messages.PayloadInitComplete ic -> Just $ Message.initCompleteParties ic
            _ -> Nothing

        case filter partyFilter parties of
          [] -> err ("[FAIL] Saw no hello for party " ++ show (Message.helloPartyId hello) ++ " in init completion")
          [h] | h /= hello -> err ("[FAIL] Party " ++ show (Message.helloPartyId hello) ++ " in init completion")
              | otherwise -> err "[OKAY] Hello ok!"
          _ -> err ("[FAIL] Multiple hello for party " ++ show (Message.helloPartyId hello) ++ "in init completion")

  where
    partyFilter h = Message.helloPartyId hello == Message.helloPartyId h







