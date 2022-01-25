{-# LANGUAGE OverloadedStrings #-}
module Mithril.Messages where

import Data.Word(Word64)
import Data.ByteString(ByteString)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Aeson as JSON
import Data.Aeson((.=), (.:))
import qualified Data.Aeson.Types as JSONT
import qualified Data.Text as Text
import Data.Text(Text)
import qualified Data.Text.Encoding as Encoding
import qualified Data.Either as Either

type PartyId = Word64
type PublicKey = Bytes
type Stake = Word64
-- TODO: this appears to be ISO8601 time, should parse accordingly?
type Time = String

type Bytes = ByteString
type Hex = ByteString

data Signature = Signature
  { signatureIndex :: Word64
  , signatureSig :: Bytes
  }
  deriving(Show, Eq)

data Participant = Participant
  { participantPartyId :: PartyId
  , participantStake :: Stake
  , participantPublicKey :: PublicKey
  }
  deriving(Show, Eq)

data Parameters = Parameters
  { parametersK :: Word64
  , parametersM :: Word64
  , parametersPhiF :: Double
  }
  deriving (Show, Eq)

data Certificate = Certificate
  { certificateId :: Word64
  , certificateNodeId :: Word64
  , certificateHash :: Bytes
  , certificatePrevHash :: Bytes
  , certificateParticipants :: [Participant]
  , certificateBlockNumber :: Word64
  , certificateBlockHash :: Bytes
  , certificateMerkleRoot :: Bytes
  , certificateMultiSig :: Bytes
  , certificateStartedAt :: Time
  , certificateFinishedAt :: Time
  }
  deriving(Show, Eq)
-------------------------------------------------------------------------------
-- Messages

newtype Message = Message
  { messagePayload :: Payload
  }
  deriving(Show, Eq)

data Payload =
    PayloadHello Hello
  | PayloadSigRequest SigRequest
  | PayloadSigResponse SigResponse
  deriving(Show, Eq)

data Hello = Hello
  { helloCardanoAddress :: String
  , helloPartyId :: PartyId
  , helloStake :: Stake
  , helloPublicKey :: PublicKey
  }
  deriving(Show, Eq)

data SigRequest = SigRequest
  { sigRequestId :: Word64
  , sigRequestParams :: Parameters
  , sigRequestParticipants :: [Participant]
  , sigRequestCert :: Certificate
  }
  deriving(Show, Eq)

data SigResponse = SigResponse
  { sigResponseRequestId :: Word64
  , sigResponseSignatures :: [Signature]
  }
  deriving(Show, Eq)

-------------------------------------------------------------------------------
-- Serializers

-- TODO: should we implement Go's `omitempty`?

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f e =
  case e of
    Left l -> Left $ f l
    Right r -> Right r

bytesFromHex :: Text -> Either String ByteString
bytesFromHex t =  Base16.decode (Encoding.encodeUtf8 t)

bytesToHex :: ByteString -> Text
bytesToHex = Encoding.decodeUtf8 . Base16.encode

bytesFromBase64 :: Text -> Either String ByteString
bytesFromBase64 t = Base64.decode (Encoding.encodeUtf8 t)

bytesToBase64 :: ByteString -> Text
bytesToBase64 = Encoding.decodeUtf8 . Base64.encode


parseTextFromEither :: String -> (Text -> Either String b) -> JSON.Value -> JSONT.Parser b
parseTextFromEither s f =
  JSON.withText s $ \t ->
    case f t of
      Left err -> fail err
      Right b -> pure b


parseBase64 :: JSON.Value -> JSONT.Parser Bytes
parseBase64 = parseTextFromEither "Bytes" bytesFromBase64

parseHex :: JSON.Value -> JSONT.Parser Hex
parseHex = parseTextFromEither "Hex" bytesFromHex

instance JSON.ToJSON Hello where
  toJSON h =
    JSON.object [ "cardano_address" .= helloCardanoAddress h
                , "party_id" .= helloPartyId h
                , "stake" .= helloStake h
                , "public_key" .= bytesToBase64 (helloPublicKey h)
                ]

instance JSON.FromJSON Hello where
  parseJSON =
    JSON.withObject "Hello" $ \h ->
      Hello <$> h .: "cardano_address"
            <*> h .: "party_id"
            <*> h .: "stake"
            <*> (h .: "public_key" >>= parseBase64)


instance JSON.ToJSON Parameters where
  toJSON p =
    JSON.object [ "k" .= parametersK p
                , "m" .= parametersM p
                , "phi_f" .= parametersPhiF p
                ]

instance JSON.FromJSON Parameters where
  parseJSON =
    JSON.withObject "Parameters" $ \p ->
      Parameters <$> p .: "k"
                 <*> p .: "m"
                 <*> p .: "phi_f"

instance JSON.ToJSON Participant where
  toJSON p =
    JSON.object [ "party_id" .= participantPartyId p
                , "stake" .= participantStake p
                , "public_key" .= bytesToBase64 (participantPublicKey p)
                ]

instance JSON.FromJSON Participant where
  parseJSON =
    JSON.withObject "Participant" $ \p ->
      Participant <$> p .: "party_id"
                  <*> p .: "stake"
                  <*> (p .: "public_key" >>= parseBase64)

instance JSON.ToJSON Certificate where
  toJSON c =
    JSON.object [ "id" .= certificateId c
                , "node_id" .= certificateNodeId c
                , "cert_hash" .= bytesToHex (certificateHash c)
                , "prev_hash" .= bytesToHex (certificatePrevHash c)
                , "participants" .= certificateParticipants c
                , "block_number" .= certificateBlockNumber c
                , "block_hash" .= bytesToHex (certificateBlockHash c)
                , "merkle_root" .= bytesToHex (certificateMerkleRoot c)
                , "multi_sig" .= bytesToBase64 (certificateMultiSig c)
                , "sig_started_at" .= certificateStartedAt c
                , "sig_finished_at" .= certificateFinishedAt c
                ]

instance JSON.FromJSON Certificate where
  parseJSON =
    JSON.withObject "Certificate" $ \c ->
      Certificate <$> c .: "id"
                  <*> c .: "node_id"
                  <*> (c .: "cert_hash" >>= parseHex)
                  <*> (c .: "cert_prev" >>= parseHex)
                  <*> c .: "participants"
                  <*> c .: "block_number"
                  <*> (c .: "block_hash" >>= parseHex)
                  <*> (c .: "merkle_root" >>= parseHex)
                  <*> (c .: "multi_sig" >>= parseBase64)
                  <*> c .: "sig_started_at"
                  <*> c .: "sig_finished_at"

instance JSON.ToJSON SigRequest where
  toJSON req =
    JSON.object [ "request_id" .= sigRequestId req
                , "params" .= sigRequestParams req
                , "participants" .= sigRequestParticipants req
                , "certificate" .= sigRequestCert req
                ]

instance JSON.FromJSON SigRequest where
  parseJSON =
    JSON.withObject "SigRequest" $ \r ->
      SigRequest <$> r .: "request_id"
                 <*> r .: "params"
                 <*> r .: "participants"
                 <*> r .: "certificate"

instance JSON.ToJSON SigResponse where
  toJSON r =
    JSON.object [ "request_id" .= sigResponseRequestId r
                , "sig" .= sigResponseSignatures r
                ]

instance JSON.FromJSON SigResponse where
  parseJSON =
    JSON.withObject "SigResponse" $ \r ->
      SigResponse <$> r .: "request_id"
                  <*> r .: "sig"

instance JSON.ToJSON Signature where
  toJSON s =
    JSON.object [ "index" .= signatureIndex s
                , "sig" .= bytesToBase64 (signatureSig s)
                ]

instance JSON.FromJSON Signature where
  parseJSON =
    JSON.withObject "Signature" $ \s ->
      Signature <$> s .: "index"
                <*> (s .: "sig" >>= parseBase64)

instance JSON.ToJSON Payload where
  toJSON p =
    case p of
      PayloadHello p -> JSON.toJSON p
      PayloadSigRequest r -> JSON.toJSON p
      PayloadSigResponse r -> JSON.toJSON p

payloadTypeName :: Payload -> Text
payloadTypeName p =
  case p of
    PayloadHello _ -> "hello"
    PayloadSigRequest _ -> "sigRequest"
    PayloadSigResponse _ -> "sigResponse"

instance JSON.ToJSON Message where
  toJSON m =
    JSON.object [ "type" .= payloadTypeName (messagePayload m)
                , "payload" .= messagePayload m
                ]

instance JSON.FromJSON Message where
  parseJSON =
    JSON.withObject "Message" $ \m ->
      do  tyV <- m .: "type"
          ty <- JSON.withText "Message Type" pure tyV
          payloadV <- m .: "payload"
          payload <- case ty of
            "hello" -> PayloadHello <$> JSON.parseJSON payloadV
            "sigRequest" -> PayloadSigRequest <$> JSON.parseJSON payloadV
            "sigResponse" -> PayloadSigResponse <$> JSON.parseJSON payloadV
            _ -> fail ("Unknown message type: '" <> Text.unpack ty <> "'")

          pure $ Message payload



