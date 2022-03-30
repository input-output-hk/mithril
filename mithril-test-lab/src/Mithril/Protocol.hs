module Mithril.Protocol where

import Data.Word(Word64)
import Data.ByteString.Lazy(ByteString)
import Data.Map(Map)
import System.Random as Random

type PartyId = Word64
type SessionId = PartyId
type PublicKey = ByteString
type SecretKey = ByteString
type Message = ByteString
type Index = Word64
type Signature = ByteString
type Stake = Word64
type PartyList = Map PartyId Stake



-------------------------------------------------------------------------------
-- Merkel Trees

type Hash = ByteString
type MerkelTreePath = [Hash]


mtCreate :: [ByteString] -> Hash
mtCreate msgParts = undefined

-- this does not seem to be completely specified
-- the paper refers to `H_p(v_i)`
-- but it's unclear exactly what that means, maybe a typo for `v`?
mtCheck :: Hash -> Int -> Int -> Int -> MerkelTreePath -> Bool
mtCheck tree size value loc pathNeighbors = undefined


-------------------------------------------------------------------------------
-- Multi signature scheme

-- TODO
data MspParams = MspParams
data CurvePoint = Int -- Not sure about this
type ProofOfPossession = (Hash, CurvePoint)
type DenseMappingValue = Int -- Not sure about this either

-- Generate keys for part of an MSP
mspGen :: Random.RandomGen r => r -> MspParams -> m (SecretKey, PublicKey, ProofOfPossession, r)
mspGen random params = undefined

-- Check that the the proof of possesion is correct
mspCheck :: PublicKey -> ProofOfPossession -> Bool
mspCheck mvk k = undefined

-- Sign a message
mspSig :: SecretKey -> Message -> Signature
mspSig sk msg = undefined

-- Verify a signature
mspVer :: Message -> PublicKey -> Signature -> Bool
mspVer msg mvk sig = undefined

-- Aggregate keys
mspAKey :: [PublicKey] -> PublicKey
mspAKey keys = undefined

-- Aggregate signatures
mspAggr :: Message -> [Signature] -> Signature
mspAggr msg sigs = undefined

mspAVer :: Message -> PublicKey -> Signature -> Bool
mspAVer = mspVer

-- dense mapping lookup?  TODO
-- cf Definition 6
mspEval :: Message -> Index -> Signature -> DenseMappingValue
mspEval = undefined


-------------------------------------------------------------------------------
-- Proof System

type ReferenceString = ByteString

data ProofStatement = ProofStatement
  { psAVKTree :: Hash
  , psKey :: PublicKey
  , psSignature :: Signature
  , psMessage :: Message
  }

data ProofWitness = ProofWitness
  { pwKey :: PublicKey
  , pwStake :: Stake
  , pwParty :: PartyId
  , pwEv :: DenseMappingValue
  , pwSignature :: Signature -- sigma (σ)
  , pwIndex :: Index
  }

-- TODO: what is the parameter here - in the case of concatenation
-- this is bottom in all cases anyhow
psReferenceString :: ReferenceString
psReferenceString = undefined

-- prove the statement given some witness
psProve :: ReferenceString -> ProofStatement -> ProofWitness -> ProofWitness
psProve rs x w = undefined

-- Verify the proof
-- NOTE: is there a typo?  did they use `w` instead of `pi` in section 2.9?
psVerify :: ReferenceString -> ProofStatement -> ProofWitness -> Bool
psVerify rs x w = undefined


-------------------------------------------------------------------------------
-- PI_STM (figure 6)

data CanError a =
    Error String
  | Value a

data CanFail a =
    Failure
  | Success a

data SigInfo = SigInfo
  { siSignature :: Signature
  , siReg :: (PublicKey, Stake)
  , siParty :: PartyId
  , siPath :: MerkelTreePath
  }

data PI_STM =
  -- Fig 6:
  -- Check if we're eligible to sign Message at Index
    EligibilityCheck Message Index
  | EligibilityCheckReturn Bool

  -- Sign Message at Index
  | CreateSig Message Index
  | CreateSigResponse SigInfo

  -- Verify a signature is valid for an index
  | Verify PartyId SigInfo Index Message
  | VerifyResponse Bool

  | Aggregate [PartyId] [SigInfo] [Index] Message
  -- NB: specified as running Verify?
  -- in this case we are supposed to produce a vector of witnesses
  -- where the description of PS doesn't seem to allow that
  | AggregateResponse (CanError (CanFail (PublicKey, Signature, ProofWitness)))

  | VerifyAggregate (PublicKey, Signature, ProofWitness) Message
  | VerifyAggregateResponse Bool


-- simple model of a REST style interaction
data ProtoMessage = ProtoMessage
  { messageFrom :: PartyId
  , messageTo :: PartyId
  , messageRequest :: PI_STM
  , messageResponse :: Maybe PI_STM  -- `Nothing` is when the message is ignored
  }




--

