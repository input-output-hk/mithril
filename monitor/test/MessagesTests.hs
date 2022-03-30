{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MessagesTests where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSONT
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty as Tasty
import qualified Data.ByteString as BS
import Data.Word(Word8, Word64)

import Mithril.Messages


tests :: Tasty.TestTree
tests =
  Tasty.testGroup "Message"
    [ roundTrip "Signature" signatureGen
    , roundTrip "Parameters" parametersGen
    , roundTrip "Participant" participantGen
    , roundTrip "Certificate" certificateGen
    , roundTrip "Hello" helloGen
    , roundTrip "SigRequest" sigRequestGen
    , roundTrip "SigResponse" sigResponseGen
    , roundTrip "Message" messageGen
    ]

roundTrip :: (Eq a, Show a, JSON.FromJSON a, JSON.ToJSON a) => String -> QC.Gen a -> Tasty.TestTree
roundTrip n p = QC.testProperty (n ++ " (round trip)") (QC.forAll p roundTripCorrect)

asciiStringGen :: QC.Gen String
asciiStringGen = QC.listOf QC.arbitraryASCIIChar

bytesGen :: QC.Gen Bytes
bytesGen = QC.arbitrary

keyGen :: QC.Gen Bytes
keyGen = bytesGen

partyIdGen :: QC.Gen Word64
partyIdGen = QC.chooseAny

indexGen :: QC.Gen Word64
indexGen = QC.chooseAny

stakeGen :: QC.Gen Word64
stakeGen = QC.chooseAny

hashGen :: QC.Gen Bytes
hashGen = bytesGen

timeGen :: QC.Gen String
timeGen = asciiStringGen

signatureGen :: QC.Gen Signature
signatureGen =
  Signature <$> indexGen
            <*> keyGen

parametersGen :: QC.Gen Parameters
parametersGen =
  Parameters <$> QC.choose (0, 1000)
             <*> QC.choose (0, 1000)
             <*> QC.choose (0, 1.0)

participantGen :: QC.Gen Participant
participantGen =
  Participant <$> partyIdGen
              <*> stakeGen
              <*> keyGen

certificateGen :: QC.Gen Certificate
certificateGen =
  Certificate <$> QC.chooseAny -- cert id
              <*> QC.chooseAny  -- node id
              <*> hashGen
              <*> hashGen
              <*> QC.listOf participantGen
              <*> QC.chooseAny  -- block id
              <*> hashGen
              <*> hashGen
              <*> bytesGen -- multisig
              <*> timeGen
              <*> timeGen

messageGen :: QC.Gen Message
messageGen = Message <$> payloadGen

payloadGen :: QC.Gen Payload
payloadGen =
  QC.oneof [ PayloadHello <$> helloGen
           , PayloadSigRequest <$> sigRequestGen
           , PayloadSigResponse <$> sigResponseGen
           ]

helloGen :: QC.Gen Hello
helloGen =
  Hello <$> asciiStringGen  -- cardano address
        <*> partyIdGen
        <*> stakeGen
        <*> keyGen

sigRequestGen :: QC.Gen SigRequest
sigRequestGen =
  SigRequest <$> QC.chooseAny -- requestId
             <*> parametersGen
             <*> QC.listOf participantGen
             <*> certificateGen

sigResponseGen :: QC.Gen SigResponse
sigResponseGen =
  SigResponse <$> QC.chooseAny -- requestId
              <*> QC.listOf signatureGen

roundTripCorrect :: 
  (Eq a, Show a, JSON.ToJSON a, JSON.FromJSON a) =>
  a ->
  QC.Property
roundTripCorrect v =
  let rv' = JSON.fromJSON (JSON.toJSON v)
  in case rv' of
    JSON.Error err -> QC.counterexample err False
    JSON.Success v' -> v QC.=== v'
