package cert

import (
	"encoding/base64"
	"encoding/hex"
	"encoding/json"
	"github.com/input-output-hk/mithril/go-node/pkg/mithril"
	"time"
)

type (
	Certificate struct {
		Id     uint64
		Params mithril.Parameters
		// Participants []mithril.Participant

		BlockNumber uint64
		BlockHash   []byte
		MerkleRoot  []byte
		MultiSig    []byte

		SigStartedAt  time.Time
		SigFinishedAt time.Time
	}
)

func (c Certificate) MarshalJSON() ([]byte, error) {
	type temp struct {
		Id          uint64 `json:"id"`
		BlockNumber uint64 `json:"block_number"`
		BlockHash   string `json:"block_hash"`
		MerkleRoot  string `json:"merkle_root"`
		MultiSig    string `json:"multi_sig"`

		SigStartedAt  time.Time `json:"sig_started_at"`
		SigFinishedAt time.Time `json:"sig_finished_at"`
	}

	t := temp{
		Id:            c.Id,
		BlockNumber:   c.BlockNumber,
		BlockHash:     hex.EncodeToString(c.BlockHash),
		MerkleRoot:    hex.EncodeToString(c.MerkleRoot),
		MultiSig:      base64.StdEncoding.EncodeToString(c.MultiSig),
		SigStartedAt:  c.SigStartedAt,
		SigFinishedAt: c.SigFinishedAt,
	}

	return json.Marshal(t)
}
