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
import Control.Monad(forM_, forever, when, unless)
import qualified Data.IORef as IORef
import Control.Exception(bracket)
import qualified Control.Concurrent as Conc

import qualified Mithril.Network as MNet
import qualified Mithril.Monitor as Monitor
import Mithril.Messages (PartyId, Stake)
import Mithril.Util(pollUntil, (++/), ping)
import qualified Mithril.Messages as M

import qualified Debug.Trace as Trace


type RMsg = MNet.RoutedMessage M.PartyId M.Message

data MOutput =
    Send RMsg
  | Err String
  | Suppress
  deriving Eq

type Monitor = Monitor.Monitor RMsg MOutput

send :: RMsg -> Monitor ()
send = Monitor.output . Send

suppress :: Monitor ()
suppress = Monitor.output Suppress

sendFromTo :: PartyId -> PartyId -> M.Message -> Monitor ()
sendFromTo from to msg = send $ MNet.RoutedMessage from to msg

err :: String -> Monitor ()
err = Monitor.output . Err

-- configure a lab and run a monitor after setting up
testMonitor :: MNet.Node RMsg -> Monitor () -> Bool -> IO ()
testMonitor labNode mon log = go labNode mon
  where
    go labNode mon =
      do  -- read any monitor output
          let (mon', out) = Monitor.getOutput mon

          -- dispatch all output produced by the monitor
          out `forM_` \case
            Send msg -> MNet.sendTo labNode msg
            Err e -> putStrLn e
            Suppress -> pure ()

          -- if monitor is complete - finish the computation
          case Monitor.getResult mon' of
            Nothing ->
              do  msg <- MNet.recvFrom labNode
                  let mon'' = mon' `Monitor.observe` msg
                      output = snd $ Monitor.getOutput mon''
                  unless (Suppress `elem` output) (MNet.sendTo labNode msg)
                  when log (printMsg msg)
                  go labNode mon''

            Just () -> pure ()


testMonitor' :: LabConfig -> Monitor () -> Bool -> IO ()
testMonitor' lc m log = withLab lc (\n -> testMonitor n m log)

defaultParams :: M.Parameters
defaultParams =
  M.Parameters { M.parametersK = 5
               , M.parametersM = 100
               , M.parametersPhiF = 0.2
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
  , netParams :: M.Parameters
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
            , labRustDir = "../mithril-proto/test-node"
            , labRestPort = 8000
            }
  where
    labNetCfg =
      NetworkConfig { netParams = defaultParams
                    , netNodes = [ localConfig 1 1 8001
                                 , localConfig 2 1 8002
                                 ]
                    }


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
testMonitorInit = testMonitor' labConfig1 monitorInit

testMonitorInitCorrupt :: Bool -> IO ()
testMonitorInitCorrupt log =
  withLab labConfig1 $ \labNode ->
    testMonitor (corruptInitComplete labNode) monitorInit log


corruptInitComplete :: MNet.Node RMsg -> MNet.Node RMsg
corruptInitComplete inner =
    inner { MNet.recvFrom = corrupt }
  where
    corrupt =
      do  msg <- MNet.recvFrom inner
          pure $ case messagePayload msg of
            M.PayloadInitComplete ic ->
              -- increment stake for all players
              let ps' = [p { M.helloStake = M.helloStake p + 1 } | p <- M.initCompleteParties ic]
              in setMessagePayload (M.PayloadInitComplete $ M.InitComplete ps') msg
            _ -> msg


monitorInit :: Monitor ()
monitorInit = go Map.empty
  where
    go :: Map M.PartyId M.Hello -> Monitor ()
    go map =
      Monitor.on $ \m ->
        case messagePayload m of
          M.PayloadHello h ->
            case Map.lookup (M.helloPartyId h) map of
              Nothing ->
                do  let map' =  Map.insert (M.helloPartyId h) h map
                        target = messageTo m
                    Monitor.allInParallel_ [go map', monitorHelloCompletesFor target h]

              Just _ -> go map
          M.PayloadInitComplete _ -> pure ()
          _ -> go map

monitorHelloCompletesFor :: M.PartyId -> M.Hello -> Monitor ()
monitorHelloCompletesFor p hello =
  Monitor.filterInput ((==p) . messageFrom) $
    do  parties <- Monitor.onJust $ \m ->
          case messagePayload m of
            M.PayloadInitComplete ic -> Just $ M.initCompleteParties ic
            _ -> Nothing

        case filter partyFilter parties of
          [] -> err ("[FAIL] Saw no hello for party " ++ show (M.helloPartyId hello) ++ " in init completion")
          [h] | h /= hello -> err ("[FAIL] Party " ++ show (M.helloPartyId hello) ++ " in init completion inconsistent with previous hello")
              | otherwise -> err "[OKAY] Hello ok!"
          _ -> err ("[FAIL] Multiple hello for party " ++ show (M.helloPartyId hello) ++ "in init completion")

  where
    partyFilter h = M.helloPartyId hello == M.helloPartyId h


printMsg :: RMsg -> IO ()
printMsg msg =
  do  putStrLn (show (MNet.rmFrom msg) ++ " --> " ++ show (MNet.rmTo msg))
      print (MNet.rmMessage msg)
      putStrLn ""


messagePayload :: RMsg -> M.Payload
messagePayload = M.messagePayload . MNet.rmMessage

setMessagePayload :: M.Payload -> RMsg -> RMsg
setMessagePayload mp r =
  r { MNet.rmMessage = M.Message mp }

messageFrom :: RMsg -> M.PartyId
messageFrom = MNet.rmFrom

messageTo :: RMsg -> M.PartyId
messageTo = MNet.rmTo
