package cert

import (
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
	return nil, nil
}
