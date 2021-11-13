package node

import (
	"github.com/input-output-hk/mithril/go-node/pkg/mithril"
)

const (
	helloMessage = "hello"
	sigRequest   = "sigRequest"
	sigResponse  = "sigResponse"
)

// Here we define a mithril node communication messages.
// All keys (signatures, multisig and mithril key pairs) will be encoded as base64

type (
	Message struct {
		Type    string      `json:"type"`
		Payload interface{} `json:"payload"`
	}

	Hello struct {
		CardanoAddress string `mapstructure:"cardano_address" json:"cardano_address"`
		PartyId        uint64 `mapstructure:"party_id" json:"party_id"`
		Stake          uint64 `mapstructure:"stake" json:"stake"`
		PublicKey      string `mapstructure:"public_key" json:"public_key"`
	}

	SigRequest struct {
		RequestId uint64 `mapstructure:"request_id" json:"request_id"`

		Params       mithril.Parameters    `mapstructure:"params" json:"cardano_address"`
		Participants []mithril.Participant `mapstructure:"participants" json:"participants"`

		BlockNumber uint64 `mapstructure:"block_number" json:"block_number"`
		BlockHash   string `mapstructure:"block_hash" json:"block_hash"`
		MerkleRoot  string `mapstructure:"merkle_root" json:"merkle_root"`
	}

	SigResponse struct {
		RequestId  uint64      `mapstructure:"request_id" json:"request_id"`
		Signatures []Signature `mapstructure:"signatures" json:"signatures"`
	}

	Signature struct {
		Index uint64 `mapstructure:"index" json:"index"`
		Sig   string `mapstructure:"sig" json:"sig"`
	}
)
