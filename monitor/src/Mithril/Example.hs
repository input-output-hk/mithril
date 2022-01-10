module Mithril.Example where

import qualified Data.Set as Set
import Data.Set(Set)
import qualified Control.Monad.Writer as Writer
import qualified Mithril.Monitor as Monitor
import qualified Mithril.Protocol as Protocol

data MessageTrace = MessageTrace
  { sender :: Protocol.PartyId
  , receiver :: Protocol.PartyId
  , request :: Protocol.PI_STM
  , response :: Maybe Protocol.PI_STM
  }

message :: Protocol.PI_STM -> Maybe Protocol.Message
message p =
  case p of
    Protocol.EligibilityCheck msg _ -> Just msg
    Protocol.CreateSig msg _ -> Just msg
    Protocol.Verify _ _ _ msg -> Just msg
    Protocol.Aggregate _ _ _ msg -> Just msg
    Protocol.VerifyAggregate _ msg -> Just msg
    _ -> Nothing

-- We can define a simple monitor that terminates when it sees a problem
-- or it stops looking

type Monitor a = Monitor.Monitor MessageTrace a

data Check =
    Ok
  | Problem String




-- define what it means for a party to be unknown
csigPartyUnknown :: Set Protocol.PartyId -> MessageTrace -> Maybe Protocol.PartyId
csigPartyUnknown pids msg =
  case response msg of
    Just (Protocol.CreateSigResponse si) ->
      if not (Protocol.siParty si `Set.member` pids)
        then Just (Protocol.siParty si)
        else Nothing
    _ -> Nothing


-- given a known list of parties, CreateSig should not respond in
-- a way that involves an unknown party
createSigUnknownParty :: Set Protocol.PartyId -> Monitor Check
createSigUnknownParty pids =
  do  party <- Monitor.onJust (csigPartyUnknown pids)
      pure $ Problem ("Unknown party id in CreateSig response: " <> show party)


-- We can also define a monitor that logs using Writer

type ProblemLog = [String]
type LoggingMonitor a = Monitor.MonitorT MessageTrace (Writer.Writer ProblemLog) a

log :: String -> LoggingMonitor a
log logmsg = Writer.tell [logmsg]

csigPartyUnknownLog :: Set Protocol.PartyId -> LoggingMonitor ()
csigPartyUnknownLog = undefined