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


messageTests :: Tasty.TestTree
messageTests =
  Tasty.testGroup "Message"
    [ QC.testProperty "Message (round trip)" (QC.forAll messageGen roundTripCorrect)
    ]

bytesGen :: QC.Gen Bytes
bytesGen = BS.pack <$> QC.listOf QC.chooseAny

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
timeGen = QC.listOf QC.chooseAny

signatureGen :: QC.Gen Signature
signatureGen =
  Signature <$> indexGen
            <*> keyGen

parameterGen :: QC.Gen Parameters
parameterGen =
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
  Hello <$> QC.listOf QC.chooseAny  -- cardano address
        <*> partyIdGen
        <*> stakeGen
        <*> keyGen

sigRequestGen :: QC.Gen SigRequest
sigRequestGen =
  SigRequest <$> QC.chooseAny -- requestId
             <*> parameterGen
             <*> QC.listOf participantGen
             <*> certificateGen

sigResponseGen :: QC.Gen SigResponse
sigResponseGen =
  SigResponse <$> QC.chooseAny -- requestId
              <*> QC.listOf signatureGen

roundTripCorrect :: 
  (Eq a, JSON.ToJSON a, JSON.FromJSON a) =>
  a ->
  Bool
roundTripCorrect v =
  let rv' = JSON.fromJSON (JSON.toJSON v)
  in case rv' of
    JSON.Error _ -> False
    JSON.Success v' -> v == v'
