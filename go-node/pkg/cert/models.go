package cert

import (
	"bytes"
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
		Id           uint64                 `json:"id" mapstructure:"id"`
		NodeId       uint64                 `json:"node_id,omitempty" mapstructure:"node_id"`
		CertHash     Hex                    `json:"cert_hash"`
		PrevHash     Hex                    `json:"prev_hash"`
		Participants []*mithril.Participant `json:"participants"`
		BlockNumber  uint64                 `json:"block_number" mapstructure:"block_id"`
		BlockHash    Hex                    `json:"block_hash"`
		MerkleRoot   Hex                    `json:"merkle_root"`
		MultiSig     Base64                 `json:"multi_sig"`

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
	buf, err := hex.DecodeString(str)
	if err != nil {
		return err
	}

	*h = buf
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

	buf, err := base64.StdEncoding.DecodeString(str)
	if err != nil {
		return err
	}

	*b64 = buf
	return nil
}

// VerifyMultiSig certificate verification method.
func (c Certificate) VerifyMultiSig(clerk *mithril.Clerk) error {
	hash := Hash(c)

	multiSig, err := mithril.MultiSigFromBytes(c.MultiSig)
	if err != nil {
		return err
	}
	return clerk.VerifyMultiSign(multiSig, hex.EncodeToString(hash))
}

func (c Certificate) VerifyHash() bool {
	return bytes.Compare(c.CertHash, Hash(c)) == 0
}
