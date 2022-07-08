{-# LANGUAGE MultiWayIf #-}
module Mithril.Process.AggregatorModel where

import qualified Mithril.Process.Network as N
import qualified Mithril.Process.Process as P
import Data.Word (Word64)
import Data.Map (Map)
import Data.Sequence (Seq ((:|>)), (<|))
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Control.Monad.State as State
import qualified Data.Maybe as Maybe
import Data.Typeable (Typeable)
import Control.Monad (when)
import qualified System.Random as Random
import Data.Foldable (traverse_)
import qualified Data.Foldable as Foldable

-- This module implements a simplified version of the aggregator protocol as well
-- as a simple scenario in which the model proceeds through the signing of a snapshot.
--
-- The implementation is split into two major parts:
--  * `aggregator` is a model of the service layer of the protocol written as a `Proc`
--  * Functions called by `aggregator` that model the state transitions of the system
--    ex. `register` and `partySign` as ordinary Haskell in the state monad
--
-- Currently it is simplified in that:
--  * it doesn't do any real crypto
--  * it doesn't implement the lottery / validate signers wrt the lottery -
--    instead it produces a signature when a simple majority of registered
--    signers provide a signature
--
-- Additionally, it should theoretically be possible for pure Procs to interact with
-- code that does IO to actual implementations but this kind of 'simulator' hasn't
-- been written yet.  The idea would be to allow the comparison of the model's behavior
-- with the behavior of an actual implementation.

-------------------------------------------------------------------------------
-- Aggregator Protocol

-- Events of interest to the aggregator
data AggregatorEvent =
    AggRegisterParty (Request Registration Bool)
  | AggSign (Request PartySignature Bool)
  | AggNewSnapshot Snapshot
  | AggGetCertificate (Request (Maybe MithrilRoundId) (Maybe GetCertResult))

-- Model of the aggregator process as a protocol
aggregator :: StakeDist -> P.RecvPort AggregatorEvent -> P.Proc ()
aggregator stakes event = main (aggInitialState stakes)
  where
    main state =
      do  e <- P.recv event
          case e of
            AggRegisterParty (Request response regist) ->
              do  say "got register"
                  let (isSuccess, state') = run (register regist) state
                  P.send response isSuccess
                  main state'

            AggSign (Request response sig) ->
              do  say "got signature"
                  let (isSuccess, state') = run (partySign sig) state
                  P.send response isSuccess
                  main state'

            AggNewSnapshot snap ->
              let (_, state') = run (newSnapshot snap) state
              in main state'

            AggGetCertificate (Request response mbId) ->
              do  let (mbCert, _) = run (getCert mbId) state
                  P.send response mbCert
                  main state

    run = State.runState
    say m = P.logmsg ("Aggregator: " ++ show m)

data GetCertResult = GetCertResult
  { getCertCertificate :: Certificate
  , getCertSignatures :: Maybe (Map PartyId (Signature Certificate))
  }
  deriving Show


-------------------------------------------------------------------------------
-- Useful abstractions

-- This is the general form of things like HTTP requests
-- `Request a b` is an interaction which has a message `a`
-- and is expecting to get a response of `b`
data Request a b = Request
  { reqResponsePort :: P.SendPort b
  , reqBody :: a
  }


-------------------------------------------------------------------------------
-- Aggregator Implementation

data RoundState = RoundState
  { rsCurrentRound :: MithrilRoundId
  , rsCert :: Certificate
  , rsSignatures :: Map PartyId (Signature Certificate)
  }

data RegistrationState = RegistrationState
  { regStateCurrentRound :: MithrilRoundId
  , regStateRegistrations :: Map PartyId Registration
  , regStateStakeDist :: StakeDist
  }

data AggregatorState = AggregatorState
  { asNextRoundId :: MithrilRoundId
  , asRegState :: RegistrationState
  , asRoundState :: Maybe RoundState
  , asSignedCerts :: Map MithrilRoundId SignedCertificate
  , asRegistrations :: Map PartyId Registration
  , asCurrentStakeDist :: StakeDist
  , asSubjectQueue :: Seq (StakeDist, Snapshot)
  }

aggInitialState :: StakeDist -> AggregatorState
aggInitialState stakes =
  AggregatorState { asNextRoundId = 1
                  , asRegState = regState
                  , asRoundState = Nothing
                  , asSignedCerts = Map.empty
                  , asCurrentStakeDist = stakes
                  , asSubjectQueue = Seq.empty
                  , asRegistrations = Map.empty
                  }
  where
    regState =
      RegistrationState { regStateCurrentRound = 1
                        , regStateRegistrations = Map.empty
                        , regStateStakeDist = stakes
                        }

type Agg = State.State AggregatorState

register :: Registration -> Agg Bool
register reg =
  do  regState <- State.gets asRegState
      let currentRoundId = regStateCurrentRound regState
          stakeLookup = Map.lookup (regPartyId reg) (regStateStakeDist regState)
          regLookup = Map.lookup (regPartyId reg) (regStateRegistrations regState)

      case (regLookup, stakeLookup) of
        -- don't allow registration for anything but the current round
        _ | currentRoundId /= regMithrilRound reg -> pure False

        -- don't allow double registration
        (Just a, _) -> pure False

        -- don't allow registration without stake
        (Nothing, Nothing) -> pure False
        (Nothing, Just r) ->
          do  let regs = regStateRegistrations regState
                  regs' = Map.insert (regPartyId reg) reg regs
                  regState' = regState { regStateRegistrations = regs' }

              State.modify (\s -> s { asRegState = regState'})
              pure True

partySign :: PartySignature -> Agg Bool
partySign sig =
  do  mbRoundState <- State.gets asRoundState
      case mbRoundState of
        -- If no round is currently happening, don't sign
        Nothing -> pure False
        Just roundState ->
          let eligibleSigners = certEligibleSigners (rsCert roundState)
              sigs = rsSignatures roundState
              partyId = sigPartyId sig

          in if -- Fail if signature is not for the current round
                | rsCurrentRound roundState /= sigMithrilRound sig -> pure False

                -- Fail if signer is not eligible to sign
                | Maybe.isNothing (Map.lookup partyId eligibleSigners) -> pure False

                -- Fail if signer tries to sign twice
                | Maybe.isJust (Map.lookup partyId sigs) -> pure False

                | otherwise ->
                    do  let sigs' = Map.insert partyId (sigSignature sig) sigs
                            roundState' = roundState { rsSignatures = sigs' }

                        State.modify (\s -> s { asRoundState = Just roundState' })
                        checkRoundComplete -- if we now have enough signatures, complete the round
                        pure True


checkRoundComplete :: Agg ()
checkRoundComplete =
  do  mbRoundState <- State.gets asRoundState
      case mbRoundState of
        Nothing -> pure ()
        Just rs ->
          let cert = rsCert rs
              eligible = certEligibleSigners cert
              stakes = certSigningDist cert
              eligibleStakes = Map.intersection stakes eligible
              eligibleStake = sum (Map.elems eligibleStakes)
              threshold = (eligibleStake `div` 2) + 1

              sigs = rsSignatures rs
              signedStake = sum $ Maybe.catMaybes [Map.lookup p eligibleStakes | p <- Map.keys sigs ]

          in  if signedStake < threshold
                then pure ()
                else completeRound rs

  where
    completeRound :: RoundState -> Agg ()
    completeRound rs =
      do  as <- State.get
          let sigs = rsSignatures rs
              cert = rsCert rs
              roundId = rsCurrentRound rs
              signedCert = SignedCertificate sigs cert
              as' = as
                    { asCurrentStakeDist = certResultDist cert
                    , asRoundState = Nothing
                    , asSignedCerts = Map.insert roundId signedCert (asSignedCerts as)
                    }

          State.put as'

          case asSubjectQueue as of
            Seq.Empty -> pure ()
            queue :|> subj ->
              do  State.modify (\s -> s { asSubjectQueue = queue })
                  newRound subj

newRound :: (StakeDist, Snapshot) -> Agg ()
newRound (subDist, subSnap) =
  do  -- Create new round id
      roundId <- State.gets asNextRoundId
      let nextRound = roundId + 1
      State.modify (\s -> s { asNextRoundId = nextRound })

      -- Stake
      signingDist <- State.gets asCurrentStakeDist

      -- Merge registration in progress and begin registraion for next round
      regState <- State.gets asRegState
      regs <- State.gets asRegistrations
      let regState' =
            RegistrationState
            { regStateCurrentRound = roundId + 1
            , regStateRegistrations = Map.empty
            , regStateStakeDist = signingDist
            }
          regs' = regStateRegistrations regState `Map.union` regs


      State.modify (\s -> s { asRegState = regState'
                            , asRegistrations = regs'
                            })

      -- build Cert for new subject and start anew round
      let cert = Certificate
            { certEligibleSigners = regs'
            , certSigningDist = signingDist
            , certResultDist = subDist
            , certSnapshot = subSnap
            , certRoundId = roundId
            }
          rs =
            RoundState
              { rsCurrentRound = roundId
              , rsSignatures = Map.empty
              , rsCert = cert
              }

      State.modify (\s -> s { asRoundState = Just rs })

      pure ()

newSnapshot :: Snapshot -> Agg ()
newSnapshot snap =
  do  dist <- State.gets asCurrentStakeDist
      newSubject (dist, snap)

newSubject :: (StakeDist, Snapshot) -> Agg ()
newSubject subj =
  do  roundInProgress <- State.gets asRoundState
      case roundInProgress of
        Just _ -> State.modify (\s -> s { asSubjectQueue = subj <| asSubjectQueue s })
        Nothing -> newRound subj

getCert :: Maybe MithrilRoundId -> Agg (Maybe GetCertResult)
getCert mbId =
  do  certs <- State.gets asSignedCerts
      mbRs <- State.gets asRoundState
      case mbId of
        Just rid ->
          case Map.lookup rid certs of
            Just (SignedCertificate sigs cert) -> pure $ Just (GetCertResult cert (Just sigs) )
            Nothing ->
              case mbRs of
                Just rs | rsCurrentRound rs == rid ->
                  pure $ Just (GetCertResult (rsCert rs) Nothing)
                _ -> pure Nothing
        Nothing ->
          case mbRs of
            Just rs -> pure $ Just (GetCertResult (rsCert rs) Nothing)
            Nothing -> pure Nothing


--

request :: (Typeable a, Typeable b, Typeable c) => P.SendPort c -> a -> (Request a b -> c) -> P.Proc b
request port a mkReq =
  do  (respSend, respRecv) <- P.newPort
      P.send port (mkReq (Request respSend a))
      P.recv respRecv

registerClient :: PartyId -> PrivateKey -> P.SendPort AggregatorEvent -> P.Proc ()
registerClient partyId privKey aggregator =
  do  say "Registering..."
      let registration = Registration { regMithrilRound = 1
                                      , regPartyId = partyId
                                      , regKey = pubKeyFor privKey
                                      }

      regSuccess <- request aggregator registration AggRegisterParty
      say ("Registration response: " ++ show regSuccess)
  where
      say msg = P.logmsg ("Party " ++ show partyId ++ ": " ++ msg)



signClient :: PartyId -> PrivateKey -> P.SendPort AggregatorEvent -> P.Proc ()
signClient partyId privKey aggregator =
  do  cert <- retry $ do say "Getting cert from aggregator..."
                         mbCert <- request aggregator (Just 1) AggGetCertificate
                         when (Maybe.isNothing mbCert) (say "Cert fetch failed - retrying")
                         pure mbCert

      say "Got cert!"

      let sig = PartySignature { sigMithrilRound = 1
                               , sigPartyId = partyId
                               , sigSignature = mkSig privKey (getCertCertificate cert)
                               }

      sigSuccess <- request aggregator sig AggSign
      say ("Signature response: " ++ show sigSuccess)

  where
    retry :: P.Proc (Maybe a) -> P.Proc a
    retry p =
      do  mbr <- p
          case mbr of
            Nothing -> retry p
            Just r -> pure r

    say msg = P.logmsg ("Party " ++ show partyId ++ ": " ++ msg)

-------------------------------------------------------------------------------
-- Basic types

type SnapshotId = Word64      -- surrogate for a snapshot
type PartyId = Word64         -- party identifier
type MithrilRoundId = Word64  -- mithril round id
type Stake = Word64           -- amount of stake

-- Fake crypto version of a public key
newtype PublicKey = PublicKey Integer
  deriving (Eq, Show)

-- Fake crypto version of a private key
newtype PrivateKey = PrivateKey Integer
  deriving (Eq, Show)

-- Derive the public key for a particular private key
pubKeyFor :: PrivateKey -> PublicKey
pubKeyFor (PrivateKey i) = PublicKey i

-- A fake crypto signature of an `a`
data Signature a = Signature PublicKey a
  deriving (Eq, Show)

-- Sign an `a` using a fake private key
mkSig :: PrivateKey -> a -> Signature  a
mkSig pk = Signature (pubKeyFor pk)

-- Validate a fake signature
validateSig :: Eq a => PublicKey -> a -> Signature a -> Bool
validateSig pubKey a sig = Signature pubKey a == sig

newtype Snapshot = Snapshot Integer
  deriving Show

data Certificate = Certificate
  { certSigningDist :: StakeDist
  , certResultDist :: StakeDist
  , certSnapshot :: Snapshot
  , certEligibleSigners :: Map PartyId Registration
  , certRoundId :: MithrilRoundId
  }
  deriving Show

data SignedCertificate =
  SignedCertificate (Map PartyId (Signature Certificate)) Certificate
  deriving Show
type StakeDist = Map PartyId Stake

data Registration = Registration
  { regKey :: PublicKey
  , regPartyId :: PartyId
  , regMithrilRound :: MithrilRoundId
  }
  deriving Show

data PartySignature = PartySignature
  { sigMithrilRound :: MithrilRoundId
  , sigPartyId :: PartyId
  , sigSignature :: Signature Certificate
  }
  deriving Show

-------------------------------------------------------------------------------
-- Scenarios

-- Run a scenario, yielding the log and the result
runScenario :: N.Network a -> ([String], a)
runScenario n =
  let (a, ns) = N.runNetwork' n
  in (Foldable.toList $ N.netLog ns, a)

-- Set the seed before doing a computation
-- ex: `runScenario . withSeed 42 $ scenario1
withSeed :: Int -> N.Network a -> N.Network a
withSeed i n = N.setRandomGen (Random.mkStdGen i) >> n

scenario1 :: N.Network (Maybe GetCertResult)
scenario1 =
  do  -- Phase 1: do setup
      (aggSend, aggRecv) <- N.mkPort
      N.execute $ register aggSend aggRecv
      g' <- N.runScheduler

      -- Phase 2: cause a snapshot to happen
      N.execute $ snapshotReady aggSend
      N.runScheduler

      result <- N.evalProcess (request aggSend (Just 1) AggGetCertificate)
      case result of
        Nothing -> pure Nothing
        Just Nothing -> pure Nothing
        Just (Just a) -> pure (Just a)

  where
    stakeDist =
      Map.fromList [ (1, 1)
                   , (2, 1)
                   , (3, 1)
                   ]

    register aggSend aggRecv =
      do  P.fork (aggregator stakeDist aggRecv)
          P.fork (registerClient 1 (PrivateKey 1) aggSend)
          P.fork (registerClient 2 (PrivateKey 2) aggSend)
          P.fork (registerClient 3 (PrivateKey 3) aggSend)

    snapshotReady aggSend =
      do  P.fork (signClient 1 (PrivateKey 1) aggSend)
          P.fork (signClient 2 (PrivateKey 2) aggSend)
          P.fork (signClient 3 (PrivateKey 3) aggSend)
          P.send aggSend (AggNewSnapshot (Snapshot 1))
