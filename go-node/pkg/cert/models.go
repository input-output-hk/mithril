package cert

import (
	"github.com/input-output-hk/mithril/go-node/pkg/mithril"
	"time"
)

type (
	Certificate struct {
		Params       mithril.Parameters
		Participants []mithril.Participant

		BlockNumber uint64
		BlockHash   []byte
		MerkleRoot  []byte
		MultiSig    []byte

		CreatedAt  time.Time
		FinishedAt time.Time
	}
)

func (c Certificate) MarshalJSON() ([]byte, error) {
	return nil, nil
}