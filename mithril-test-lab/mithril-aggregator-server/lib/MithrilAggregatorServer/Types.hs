{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module MithrilAggregatorServer.Types (
  Beacon (..),
  Certificate (..),
  Error (..),
  Protocol (..),
  Signer (..),
  SignerWithStake (..),
  SingleSignature (..),
  Snapshot (..),
  Stake (..),
  ) where

import Data.Data (Data)
import Data.UUID (UUID)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time
import Data.Swagger (ToSchema, declareNamedSchema)
import qualified Data.Swagger as Swagger
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Function ((&))


-- | Beacon represents a point in the Cardano chain at which a Mithril certificate should be produced
data Beacon = Beacon
  { beaconNetwork :: Text -- ^ Cardano network
  , beaconEpoch :: Integer -- ^ Cardano chain epoch number
  , beaconBlock :: Integer -- ^ Cardano chain block number
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Beacon where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "beacon")
instance ToJSON Beacon where
  toJSON = genericToJSON (removeFieldLabelPrefix False "beacon")


-- | Certificate represents a Mithril certificate embedding a Mithril STM multisignature
data Certificate = Certificate
  { certificateHash :: Text -- ^ Hash of the current certificate
  , certificatePreviousUnderscorehash :: Text -- ^ Hash of the previous certificate
  , certificateBlock :: Integer -- ^ Cardano chain block number
  , certificateProtocol :: Protocol -- ^ 
  , certificateDigest :: Text -- ^ Digest that is signed by the signer participants
  , certificateStartedUnderscoreat :: UTCTime -- ^ Date and time at which the certificate was initialized and ready to accept single signatures from signers
  , certificateCompletedUnderscoreat :: UTCTime -- ^ Date and time at which the certificate was completed (when the quorum of single signatures was reached so that a multisignature could be aggregated from them)
  , certificateParticipants :: [SignerWithStake] -- ^ The list of the participants (potential signers) with their stakes and verification keys
  , certificateMultisignature :: Text -- ^ STM multisignature created from a quorum of single signatures from the signers
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Certificate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "certificate")
instance ToJSON Certificate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "certificate")


-- | 
data Error = Error
  { errorCode :: Value -- ^ error code
  , errorMessage :: Text -- ^ error message
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Error where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "error")
instance ToJSON Error where
  toJSON = genericToJSON (removeFieldLabelPrefix False "error")


-- | Protocol cryptographic parameters
data Protocol = Protocol
  { protocolK :: Integer -- ^ Quorum parameter
  , protocolM :: Integer -- ^ Security parameter (number of lotteries)
  , protocolPhiUnderscoref :: Float -- ^ f in phi(w) = 1 - (1 - f)^w, where w is the stake of a participant
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Protocol where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "protocol")
instance ToJSON Protocol where
  toJSON = genericToJSON (removeFieldLabelPrefix False "protocol")


-- | Signer represents a signing participant in the network
data Signer = Signer
  { signerPartyUnderscoreid :: Integer -- ^ The unique identifier of the signer
  , signerVerificationUnderscorekey :: Text -- ^ The public key used to authenticate signer signature
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Signer where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "signer")
instance ToJSON Signer where
  toJSON = genericToJSON (removeFieldLabelPrefix False "signer")


-- | Signer represents a signing party in the network (including its stakes)
data SignerWithStake = SignerWithStake
  { signerWithStakePartyUnderscoreid :: Integer -- ^ The unique identifier of the signer
  , signerWithStakeVerificationUnderscorekey :: Text -- ^ The public key used to authenticate signer signature
  , signerWithStakeStake :: Integer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SignerWithStake where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "signerWithStake")
instance ToJSON SignerWithStake where
  toJSON = genericToJSON (removeFieldLabelPrefix False "signerWithStake")


-- | SingleSignature represents a single signature originating from a participant in the network for a digest at a specific lottery index
data SingleSignature = SingleSignature
  { singleSignaturePartyUnderscoreid :: Integer -- ^ The unique identifier of the signer
  , singleSignatureIndex :: Integer -- ^ The index of the lottery won that lead to the single signature
  , singleSignatureSignature :: Text -- ^ The single signature of the digest
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SingleSignature where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "singleSignature")
instance ToJSON SingleSignature where
  toJSON = genericToJSON (removeFieldLabelPrefix False "singleSignature")


-- | Snapshot represents a snapshot file and its metadata
data Snapshot = Snapshot
  { snapshotDigest :: Text -- ^ Digest that is signed by the signer participants
  , snapshotCertificateUnderscorehash :: Text -- ^ Hash of the associated certificate
  , snapshotSize :: Integer -- ^ Size of the snapshot file in Bytes
  , snapshotCreatedUnderscoreat :: UTCTime -- ^ Date and time at which the snapshot was created
  , snapshotLocations :: [Text] -- ^ Locations where the binary content of the snapshot can be retrieved
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Snapshot where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "snapshot")
instance ToJSON Snapshot where
  toJSON = genericToJSON (removeFieldLabelPrefix False "snapshot")


-- | Stake represents the stakes of a participant in the Cardano chain
data Stake = Stake
  { stakeStake :: Integer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Stake where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "stake")
instance ToJSON Stake where
  toJSON = genericToJSON (removeFieldLabelPrefix False "stake")


uncapitalize :: String -> String
uncapitalize (first:rest) = Char.toLower first : rest
uncapitalize [] = []

-- | Remove a field label prefix during JSON parsing.
--   Also perform any replacements for special characters.
--   The @forParsing@ parameter is to distinguish between the cases in which we're using this
--   to power a @FromJSON@ or a @ToJSON@ instance. In the first case we're parsing, and we want
--   to replace special characters with their quoted equivalents (because we cannot have special
--   chars in identifier names), while we want to do vice versa when sending data instead.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = uncapitalize . fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars
    }
  where
    replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
    specialChars =
      [ ("@", "'At")
      , ("\\", "'Back_Slash")
      , ("<=", "'Less_Than_Or_Equal_To")
      , ("\"", "'Double_Quote")
      , ("[", "'Left_Square_Bracket")
      , ("]", "'Right_Square_Bracket")
      , ("^", "'Caret")
      , ("_", "'Underscore")
      , ("`", "'Backtick")
      , ("!", "'Exclamation")
      , ("#", "'Hash")
      , ("$", "'Dollar")
      , ("%", "'Percent")
      , ("&", "'Ampersand")
      , ("'", "'Quote")
      , ("(", "'Left_Parenthesis")
      , (")", "'Right_Parenthesis")
      , ("*", "'Star")
      , ("+", "'Plus")
      , (",", "'Comma")
      , ("-", "'Dash")
      , (".", "'Period")
      , ("/", "'Slash")
      , (":", "'Colon")
      , (";", "'Semicolon")
      , ("{", "'Left_Curly_Bracket")
      , ("|", "'Pipe")
      , ("<", "'LessThan")
      , ("!=", "'Not_Equal")
      , ("=", "'Equal")
      , ("}", "'Right_Curly_Bracket")
      , (">", "'GreaterThan")
      , ("~", "'Tilde")
      , ("?", "'Question_Mark")
      , (">=", "'Greater_Than_Or_Equal_To")
      , ("~=", "'Tilde_Equal")
      ]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer =
      if forParsing
        then flip T.replace
        else T.replace
