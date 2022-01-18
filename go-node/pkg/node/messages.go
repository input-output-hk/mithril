package node

import (
	"github.com/input-output-hk/mithril/go-node/internal/log"
	"github.com/input-output-hk/mithril/go-node/pkg/cert"
	"github.com/input-output-hk/mithril/go-node/pkg/mithril"
	"github.com/mitchellh/mapstructure"
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

		Params       mithril.Parameters     `mapstructure:"params" json:"params"`
		Participants []*mithril.Participant `mapstructure:"participants" json:"participants"`

		Cert cert.Certificate `json:"certificate"`
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

func readMessage(p *PeerNode, input Message, output interface{}) error {
	err := mapstructure.Decode(input.Payload, output)
	if err != nil {
		log.Errorw("Failed to decode message",
			"peer_id", p.Id(),
			"msg_type", input.Type,
			"err", err,
		)
	}
	return err
}
