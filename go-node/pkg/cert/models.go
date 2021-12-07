package cert

import (
	"encoding/base64"
	"encoding/hex"
	"encoding/json"
	"github.com/input-output-hk/mithril/go-node/pkg/mithril"
	"time"
)

type (
	Hex    []byte
	Base64 []byte

	Certificate struct {
		Id           uint64                `json:"id"`
		Params       mithril.Parameters    `json:"params"`
		Participants []mithril.Participant `json:"participants"`

		BlockNumber uint64 `json:"block_number"`
		BlockHash   Hex    `json:"block_hash"`
		MerkleRoot  Hex    `json:"merkle_root"`
		MultiSig    Base64 `json:"multi_sig"`

		SigStartedAt  time.Time `json:"sig_started_at"`
		SigFinishedAt time.Time `json:"sig_finished_at"`
	}
)

func (h Hex) MarshalJSON() ([]byte, error) {
	return json.Marshal(hex.EncodeToString(h))
}

func (h Hex) String() string {
	return hex.EncodeToString(h)
}

func (h *Hex) UnmarshalJSON(src []byte) error {
	var str string

	err := json.Unmarshal(src, &str)
	if err != nil {
		return err
	}
	bytes, err := hex.DecodeString(str)
	if err != nil {
		return err
	}

	*h = bytes
	return nil
}

func (b64 Base64) MarshalJSON() ([]byte, error) {
	return json.Marshal(base64.StdEncoding.EncodeToString(b64))
}

func (b64 *Base64) UnmarshalJSON(src []byte) error {
	var str string
	err := json.Unmarshal(src, &str)
	if err != nil {
		return err
	}

	bytes, err := base64.StdEncoding.DecodeString(str)
	if err != nil {
		return err
	}

	*b64 = bytes
	return nil
}

// VerifyMultiSig certificate verification method.
func (c Certificate) VerifyMultiSig() error {
	var participants []mithril.Participant
	for _, p := range c.Participants {
		participants = append(participants, mithril.NewParticipant(p.PartyId, p.Stake, p.PublicKey))
	}

	clerk := mithril.NewClerk(c.Params, participants)
	multiSig := mithril.MultiSigFromBytes(c.MultiSig)
	message := base64.StdEncoding.EncodeToString(c.MerkleRoot)

	return clerk.VerifyMultiSign(multiSig, message)
}
