package node

import (
	"github.com/input-output-hk/mithril/go-node/pkg/mithril"
)

const (
	helloMessage = "hello"
)

// Here we define a mithril node communication messages.
// All keys (signatures, multisig and mithril key pairs) will be encoded as base64

type (
	Message struct {
		Type    string      `json:"type"`
		Payload interface{} `json:"payload"`
	}

	Hello struct {
		// Are these parameters fixed or sign process depended?

		//PartyId   uint64 `mapstructure:"partyId" json:"partyId"`
		//Stake     uint64 `mapstructure:"stake" json:"stake"`

		CardanoAddress uint64
		PublicKey      string `mapstructure:"publicKey" json:"publicKey"`
	}

	SignRequest struct {
		RequestId uint64

		Params       mithril.Parameters
		Participants []mithril.Participant

		BlockNumber uint64
		BlockHash   string
		MerkleRoot  string
	}

	SignResponse struct {
		RequestId  uint64
		Signatures []Signature
	}

	Signature struct {
		Index uint64
		Sig   string
	}
)
