{-# LANGUAGE OverloadedStrings #-}
module Mithril.Monitor.Aggregator where

import Data.Text(Text)
import Data.Foldable (traverse_, find)
import Control.Monad.Trans(lift)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Catch(MonadThrow)
import Control.Monad.Except(ExceptT, mapExceptT, catchError, throwError, MonadError (throwError), withExceptT)
import qualified Control.Concurrent as Conc

import qualified Servant.Server as SServer
import qualified Servant.Client as SClient
import qualified Data.Aeson as JSON

import qualified MithrilAggregatorServer.API as API
import qualified MithrilAggregatorServer.Types as AT

import Mithril.Monitor.Monitor(Monitor, (<|*|>))
import qualified Mithril.Monitor.Monitor as Monitor
import Data.Void (Void, absurd)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (unless)

data RequestResponse =
    ReqCertHashGet Text
  | RespCertHashGet AT.Certificate
  | ReqCertPendingGet
  | RespCertPendingGet JSON.Value

  | ReqRegisterSignaturePost [AT.SingleSignature]
  | RespRegisterSignaturePost
  | ReqRegisterSignerPost AT.Signer
  | RespRegisterSignerPost

  | ReqSnapshotDigestGet Text
  | RespSnapshotDigestGet AT.Snapshot
  | ReqSnapshotsGet
  | RespSnapshotsGet [AT.Snapshot]
  | RespException


-- HTTP has an implicit notion of message id
-- i.e. being able to tell what message goes with what
--      response
-- we represent this here with a surrogate ID
data Message = Message
  { msgId :: Int
  , msgContent :: RequestResponse
  }

data MonitorContext i o r m = MonitorContext
  { ctxMonitorObserve :: i -> m (Monitor.Output o) -- atomic observe + modify
  , ctxOutput :: o -> m ()                         -- output function
  , ctxNewMsgId :: m Int                           -- create a message id
  }

-- Context implementation using IO
ioContext :: MonadIO m => Monitor i o a -> (o -> IO ()) -> m (MonitorContext i o a m)
ioContext mon output =
  do  monitorVar <- liftIO $ Conc.newMVar mon
      idVar <- liftIO $ Conc.newMVar 0

      let newId = liftIO $ Conc.modifyMVar idVar nextId
          nextId i = pure (i, i + 1)
          observe i m =
            let (m', o) = m `Monitor.observe` i
            in pure (o, m')
          obs i = liftIO $ Conc.modifyMVar monitorVar (observe i)


      pure MonitorContext { ctxNewMsgId = newId
                          , ctxMonitorObserve = obs
                          , ctxOutput = liftIO . output
                          }

monitorBackend ::
  Monad m =>
  API.MithrilAggregatorServerBackend (ExceptT e m) ->
  MonitorContext Message Text a m ->
  API.MithrilAggregatorServerBackend (ExceptT e m)
monitorBackend backend ctx =
  API.MithrilAggregatorServerBackend
    { API.certificateCertificateHashGet = certificateCertificateHashGet
    , API.certificatePendingGet = certificatePendingGet
    , API.registerSignaturesPost = registerSignaturesPost
    , API.registerSignerPost = registerSignerPost
    , API.snapshotDigestGet = snapshotDigestGet
    , API.snapshotsGet = snapshotsGet
    }
  where
    monitor msg =
      do  out <- ctxMonitorObserve ctx msg
          ctxOutput ctx `traverse_` Monitor.outValues out

    -- withMonitoring :: (req -> Message) -> (resp -> Message) -> (req -> m resp) -> req -> m resp

    monitorCall freq fresp handler req =
      do  mid <- lift $ ctxNewMsgId ctx
          lift $ monitor (Message mid $ freq req)

          let doCall = do  response <- handler backend req
                           lift $ monitor (Message mid $ fresp response)
                           pure response
              doErr e = do  lift $ monitor (Message mid RespException)
                            throwError e

          doCall `catchError` doErr


    monitorQuery msg fresp prod =
      do  mid <- lift $  ctxNewMsgId ctx
          lift $ monitor (Message mid msg)

          let doCall = do response <- prod backend
                          lift $ monitor (Message mid $ fresp response)
                          pure response

              doErr e = do  lift $ monitor (Message mid RespException)
                            throwError e

          doCall `catchError` doErr


    certificateCertificateHashGet =
      monitorCall ReqCertHashGet
                  RespCertHashGet
                  API.certificateCertificateHashGet

    certificatePendingGet =
      monitorQuery ReqCertPendingGet
                   RespCertPendingGet
                   API.certificatePendingGet

    registerSignaturesPost =
      monitorCall ReqRegisterSignaturePost
                  (const RespRegisterSignaturePost)
                  API.registerSignaturesPost

    registerSignerPost =
      monitorCall ReqRegisterSignerPost
                  (const RespRegisterSignaturePost)
                  API.registerSignerPost

    snapshotDigestGet =
      monitorCall ReqSnapshotDigestGet
                  RespSnapshotDigestGet
                  API.snapshotDigestGet

    snapshotsGet =
      monitorQuery ReqSnapshotsGet
                   RespSnapshotsGet
                   API.snapshotsGet


-- runMonitorRelay :: Monitor Message Text  a -> API.Config -> API.Config -> IO ()
-- runMonitorRelay monitor clientConfig serverConfig =
--     API.runMithrilAggregatorServerServer serverConfig backend
--   where
--     backend =
--       API.MithrilAggregatorServerBackend
--         { API.certificateCertificateHashGet = withClient API.certificateCertificateHashGet
--         }

--     client = API.createMithrilAggregatorServerClient
--     withClient f =
--       withExceptT (const SServer.err500)
--                   (API.runMithrilAggregatorServerClient clientConfig (f client))


-- clientAsServerErrors :: ExceptT ClientError IO -> ExceptT ServerError IO
-- clientAsServerErrors = mapExcepT mapExceptions
--   where
--     mapExceptions ce =
--       do  a <- ce
--           case a of
--             Right a' -> a'
--             Left err ->


-- Monitor Impl


-- base message monitors

reqCertHashGet :: Monitor Message a Text
reqCertHashGet =
  do  ReqCertHashGet hash <- msgContent <$> Monitor.next
      pure hash

respCertHashGet :: Monitor Message a AT.Certificate
respCertHashGet =
  do  RespCertHashGet cert <- msgContent <$> Monitor.next
      pure cert

reqCertPendingGet :: Monitor Message a ()
reqCertPendingGet =
  do  ReqCertPendingGet <- msgContent <$> Monitor.next
      pure ()

respCertPendingGet :: Monitor Message a JSON.Value
respCertPendingGet =
  do  RespCertPendingGet val <- msgContent <$> Monitor.next
      pure val

reqRegisterSignaturePost :: Monitor Message a [AT.SingleSignature]
reqRegisterSignaturePost =
  do  ReqRegisterSignaturePost sigs <- msgContent <$> Monitor.next
      pure sigs

respRegisterSignaturePost :: Monitor Message a ()
respRegisterSignaturePost =
  do  RespRegisterSignaturePost <- msgContent <$> Monitor.next
      pure ()

reqRegisterSignerPost :: Monitor Message a AT.Signer
reqRegisterSignerPost =
  do  ReqRegisterSignerPost signer <- msgContent <$> Monitor.next
      pure signer

respRegisterSignerPost :: Monitor Message a ()
respRegisterSignerPost =
  do  RespRegisterSignerPost <- msgContent <$> Monitor.next
      pure ()

reqSnapshotDigestGet :: Monitor Message a Text
reqSnapshotDigestGet =
  do  ReqSnapshotDigestGet req <- msgContent <$> Monitor.next
      pure req

respSnapshotDigestGet :: Monitor Message a AT.Snapshot
respSnapshotDigestGet =
  do  RespSnapshotDigestGet snapshot <- msgContent <$> Monitor.next
      pure snapshot

reqSnapshotsGet :: Monitor Message a ()
reqSnapshotsGet =
  do  ReqSnapshotsGet <- msgContent <$> Monitor.next
      pure ()

respSnapshotsGet :: Monitor Message a [AT.Snapshot]
respSnapshotsGet =
  do  RespSnapshotsGet snaps <- msgContent <$> Monitor.next
      pure snaps

respException :: Monitor Message a ()
respException =
  do  RespException <- msgContent <$> Monitor.next
      pure ()


withReqId :: Monitor Message a b -> Monitor Message a (Int, b)
withReqId = Monitor.both (msgId <$> Monitor.next)

responseTo :: Int -> Monitor Message a ()
responseTo mid =
  do  m <- Monitor.next
      Monitor.require (msgId m == mid)
      pure ()

--

onResponse :: Int -> Monitor Message a b -> Monitor Message a b
onResponse mid m = snd <$> Monitor.on (Monitor.both (responseTo mid) m)

data SigningContext = SigningContext
  { scSigner :: AT.Signer
  , scPending :: JSON.Value
  , scEpoch :: Epoch
  }

type Index = Integer
type PartyId = Integer
type Stake = Integer
type StakeDist = Map PartyId Stake
type Epoch = Integer
type Hash = Text
type PendingCert = JSON.Value -- this is how it appears in the generated servant code

-- Stuff we don't exactly know how to do yet

lottery :: SigningContext -> [Index]
lottery = undefined

areSignaturesSufficient :: [AT.SingleSignature] -> Bool
areSignaturesSufficient = undefined

epochChanged :: Monitor Message a Epoch
epochChanged = undefined

stakeDistForEpoch :: Epoch -> StakeDist
stakeDistForEpoch = undefined

pendingCertHash :: PendingCert -> Hash
pendingCertHash = undefined

registration :: PartyId -> Monitor Message a AT.Signer
registration pid =
  do  (reqId, signer) <- Monitor.on (withReqId reqRegisterSignerPost >>= forMyPid)
      onResponse reqId respRegisterSignerPost
      pure signer
  where
    forMyPid (a, signer) =
      Monitor.require (pid == AT.signerPartyUnderscoreid signer) >> pure (a, signer)

-- TODO: we don't know who is asking for the pending cert - does it matter?
getPendingCert :: PartyId -> Monitor Message a JSON.Value
getPendingCert pid =
  do  (reqId, _) <- withReqId reqCertPendingGet
      onResponse reqId respCertPendingGet

expectSig :: PartyId -> Index -> Monitor Message Text AT.SingleSignature
expectSig pid idx = sign
  where
    sign =
      do  (rid, sigs) <- Monitor.on (withReqId reqRegisterSignaturePost)
          sig <- findSig sigs
          onResponse rid respRegisterSignaturePost
          pure sig

    findSig sigs =
      case find theSig sigs of
        Nothing -> fail ""
        Just s -> pure s

    theSig ssig =
      AT.singleSignaturePartyUnderscoreid ssig == pid &&
      AT.singleSignatureIndex ssig == idx

-- Monitor for any particular signer
-- TODO: could add timeout here?
signer :: PartyId -> Monitor Message Text [AT.SingleSignature]
signer pid =
  do  -- register
      reg <- registration pid

      -- wait for epoch change
      epoch <- Monitor.on epochChanged

      -- wait until we have successfully acquired the pending cert
      pendingCert <- Monitor.on (getPendingCert pid)

      -- do the signing lottery
      let sc = SigningContext reg pendingCert epoch
          signatureIdxs = lottery sc

      -- wait for all signatures are produced for relevant indexes
      Monitor.all (expectSig pid <$> signatureIdxs)


-- wait until we have a signature for every index from the set of potential
-- signers
signatures :: [PartyId] -> Monitor Message Text [AT.SingleSignature]
signatures pids = withPendingConsistent getSignatures
  where
    getSignatures = go (signer <$> pids) []
    go ms acc =
      do  (sigs, ms') <- Monitor.watch ms
          let sigs' = concat sigs ++ acc
          if areSignaturesSufficient sigs'
            then pure sigs'
            else
              case ms' of
                [] -> fail "Could not get enough signatures"
                _ -> go ms' acc

-- require the pending certificate to be consistent throughout
withPendingConsistent :: Monitor Message Text a -> Monitor Message Text a
withPendingConsistent m = consistentPending `Monitor.guard` m

consistentPending :: Monitor Message Text Void
consistentPending =
  do  firstPending <- Monitor.on reqCertPendingGet
      Monitor.always (requireConsistentWith firstPending)
  where
    requireConsistentWith p1 =
      do  p2 <- Monitor.on reqCertPendingGet
          Monitor.require (isConsistentWith p1 p2)

    isConsistentWith p1 p2 = p1 == p2 -- NOTE: this isn't true because signatures are added


pendingCertIsReal :: JSON.Value -> Monitor Message Text ()
pendingCertIsReal pcert =
  do  hash <- Monitor.on reqCertHashGet
      if pendingCertHash pcert == hash
        then do cert <- Monitor.on respCertHashGet
                unless (consistent cert pcert) (Monitor.output "Inconsistent response to reqCertHash")
        else pure ()
  where
    consistent = undefined  -- TODO!


monitorUntil ::  Monitor Message a Void -> Monitor Message a b -> Monitor Message a b
monitorUntil m1 = Monitor.race (absurd <$> m1)

aggregator :: Monitor Message Text ()
aggregator =
  do  -- epoch e-1 (aka e0)
      e0 <- epochChanged
      let stakeDist = stakeDistForEpoch e0

      -- registration begins and (after the epoch boundary for e) the signatures are aggregated
      (sigs, pendingCert, e1) <- (,,) <$> signatures (Map.keys stakeDist)
                                      <|*|> Monitor.on respCertPendingGet
                                      <|*|> Monitor.on epochChanged


      -- in epoch e+1 (aka e2) we should see the pendingCert become real
      e2 <- Monitor.on epochChanged


      -- this should probably be true at all points after this, but
      -- to make sure we don't accumulate state forever we'll stop watching
      -- when the next epoch happens
      Monitor.everywhere (pendingCertIsReal pendingCert) `monitorUntil` epochChanged

      pure ()


-- -- it would be simpler to wait for all possible signers
-- allSigners :: [PartyId] -> Monitor Message Text [AT.SingleSignature]
-- allSigners pids =
--   do  -- is it possible to finish when he have a sufficient number of sigs?
--       signatures <- concat <$> Monitor.all (signer <$> pids)
--       if areSignaturesSufficient signatures
--         then pure signatures  -- at this point we should aggregate?
--         else Monitor.output "Signature failed" >> fail ""


