{-# LANGUAGE OverloadedStrings #-}
module Mithril.Messages where

import Data.Word(Word64)
import Data.ByteString.Lazy(ByteString)
import qualified Data.Aeson as JSON
import Data.Aeson((.=))

type PartyId = Word64
type PublicKey = ByteString
type Stake = Word64
type Time = String

data SigResponse = SigResponse
  { sigResponseRequestId :: Word64
  , sigResponseSignatures :: [Signature]
  }

data Signature = Signature
  { signatureIndex :: Word64
  , signatureSig :: ByteString
  }

data Participant = Participant
  { participantPartyId :: PartyId
  , participantStake :: Stake
  , participantPublicKey :: PublicKey
  }

data Parameters = Parameters
  { parametersK :: Word64
  , parametersM :: Word64
  , parametersPhiF :: Double
  }

data Certificate = Certificate
  { certificateId :: Word64
  , certificateNodeId :: Word64
  , certificateHash :: ByteString
  , certificatePrevHash :: ByteString
  , certificateParticipants :: [Participant]
  , certificateBlockNumber :: Word64
  , certificateBlockHash :: ByteString
  , certificateMerkelRoot :: ByteString
  , certificateMultiSig :: ByteString
  , certificateStartedAt :: Time
  , certificateFinishedAt :: Time
  }
-------------------------------------------------------------------------------
-- Messages

data Message = Message
  { messageType :: String
  , messagePayload :: Payload
  }

data Payload =
    PayloadHello Hello
  | PayloadSigRequest SigRequest
  | PayloadSigResponse SigResponse

data Hello = Hello
  { helloCardanoAddress :: String
  , helloPartyId :: PartyId
  , helloStake :: Stake
  , helloPublicKey :: PublicKey
  }

data SigRequest = SigRequest
  { sigRequestId :: Word64
  , sigRequestParams :: Parameters
  , sigRequestParticipants :: [Participant]
  , sigRequestCert :: Certificate
  }

-------------------------------------------------------------------------------
-- Serializers

-- TODO: base64 encode
encodeBytes :: ByteString -> JSON.Value
encodeBytes = undefined

instance JSON.ToJSON Hello where
  toJSON h =
    JSON.object [ "cardano_address" .= helloCardanoAddress h
                , "party_id" .= helloPartyId h
                , "stake" .= helloStake h
                , "public_key" .= encodeBytes (helloPublicKey h)
                ]