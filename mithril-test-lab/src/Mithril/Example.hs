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
  , time :: Int
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

type Monitor a = Monitor.Monitor MessageTrace String a

data Check =
    Ok
  | Problem String
  deriving (Show, Eq)


notOk :: Check -> Bool
notOk = (/=Ok)

-- define what it means for a party to be unknown
csigRespPartyUnknown :: Set Protocol.PartyId -> MessageTrace -> Maybe Protocol.PartyId
csigRespPartyUnknown pids msg =
  case response msg of
    Just (Protocol.CreateSigResponse si) ->
      if not (Protocol.siParty si `Set.member` pids)
        then Just (Protocol.siParty si)
        else Nothing
    _ -> Nothing


-- given a known list of parties, CreateSig should not respond in
-- a way that involves an unknown party
createSigUnknownPartyResponse :: Set Protocol.PartyId -> Monitor Check
createSigUnknownPartyResponse pids =
  do  party <- Monitor.onJust (csigRespPartyUnknown pids)
      pure $ Problem ("Unknown party id in CreateSig response: " <> show party)

-- the stake in the response of `CreateSig` should be consistent with subsequent use:
inconsistentStake :: Monitor Check
inconsistentStake =
  do  sig <- Monitor.onJust createSigResponse
      let pid = Protocol.siParty sig
          stake = snd . Protocol.siReg $ sig

      Monitor.nextWhen (stakeIncorrectForUser stake pid)
      pure $ Problem ("Inconsistent stake for user " <> show pid)


-- scope a monitor to a particular message
messageMonitor :: Set Protocol.PartyId -> Monitor Check
messageMonitor pids =
  do  msg <- Monitor.onJust (message . request)  -- TODO: version of this that doesn't consume the incoming message
      Monitor.filterInput ((==Just msg) . message . request) $

        -- run these in parallel, since they both return problems - return the first problem
        Monitor.anyInParallel [ createSigUnknownPartyResponse pids
                              , inconsistentStake
                              ]

createSigReq :: MessageTrace -> Maybe MessageTrace
createSigReq msg =
  case request msg of
    Protocol.CreateSig _ _ -> Just msg
    _ -> Nothing

createSigResponse :: MessageTrace -> Maybe Protocol.SigInfo
createSigResponse msg =
  case response msg of
    Just (Protocol.CreateSigResponse si) -> Just si
    _ -> Nothing

aggregateSigRequest :: MessageTrace -> Maybe ([Protocol.PartyId], [Protocol.SigInfo], [Protocol.Index], Protocol.Message)
aggregateSigRequest msg =
  case request msg of
    Protocol.Aggregate ps sigs idxs msg -> Just (ps, sigs, idxs, msg)
    _ -> Nothing

stakeIncorrectForUser :: Protocol.Stake -> Protocol.PartyId -> MessageTrace -> Bool
stakeIncorrectForUser stake user mt =
  case request mt of
    Protocol.Aggregate _ sigs _ _ ->
      any (/=stake) [snd . Protocol.siReg $ sig | sig <- sigs, Protocol.siParty sig == user]
