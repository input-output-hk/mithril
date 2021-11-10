package node

import "github.com/input-output-hk/mithril/go-node/pkg/mithril"

const (
	helloMessage = "hello"
)

type Message struct {
	Type    string      `json:"type"`
	Payload interface{} `json:"payload"`
}

type Hello struct {
	PartyId   uint64 `mapstructure:"partyId" json:"partyId"`
	Stake     uint64 `mapstructure:"stake" json:"stake"`
	PublicKey string `mapstructure:"publicKey" json:"publicKey"`
}

type SignRequest struct {
	RequestId uint64

	Params       mithril.StmParameters
	Participants []mithril.Participant

	BlockNumber uint64
	BlockHash   string
	MerkleRoot  string
}

type SignResponse struct {
	RequestId uint64

	Index int64
	Signature string

}
