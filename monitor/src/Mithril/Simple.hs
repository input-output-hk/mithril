{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Mithril.Simple where

import qualified Data.List as List
import Data.Text(Text)
import qualified Data.Text.IO as TIO
import Data.Map(Map)
import Data.Maybe(maybeToList, isNothing)
import Data.Foldable(foldl')
import qualified System.Process as Process
import qualified Data.Aeson as JSON
import Data.Aeson((.=))
import qualified System.Directory as Dir
import qualified System.IO.Temp as Temp
import Control.Monad(forM_)
import qualified Data.IORef as IORef
import Control.Exception(bracket)

import qualified Mithril.Network as MNet
import qualified Mithril.Monitor as Monitor
import qualified Mithril.Messages as Messages
import Mithril.Messages (PartyId, Stake)
import Mithril.Util(pollUntil, (++/), ping)


type RMsg = MNet.RoutedMessage Messages.PartyId Messages.Message

data MOutput =
    Send RMsg
  | Err Text

type Monitor = Monitor.Monitor RMsg MOutput

send :: RMsg -> Monitor ()
send = Monitor.log . Send

sendFromTo :: PartyId -> PartyId -> Messages.Message -> Monitor ()
sendFromTo from to msg = send $ MNet.RoutedMessage from to msg

err :: Text -> Monitor ()
err = Monitor.log . Err

-- configure a lab and run a monitor after setting up
testMonitor :: LabConfig -> Monitor () -> IO ()
testMonitor lc mon =
  do  withLab lc (`go` mon)

  where
    go labNode mon =
      do  -- read any monitor output
          let (mon', out) = Monitor.output mon

          -- dispatch all output produced by the monitor
          out `forM_` \case
            Send msg -> MNet.sendTo labNode msg
            Err e -> TIO.putStrLn e

          -- if monitor is complete
          case Monitor.getResult mon' of
            Nothing ->
              do  msg <- MNet.recvFrom labNode
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
route nc _ to = nConfigExtAddress <$> nodeConfig
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


test1 :: IO ()
test1 =
  do  withLab labConfig1 $ \lc ->
        do  msg <- MNet.recvFrom lc
            print msg
            _ <- getLine
            pure ()


