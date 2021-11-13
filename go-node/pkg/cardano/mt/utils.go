package mt

import (
	"crypto/sha256"
	"encoding/json"
	"github.com/input-output-hk/mithril/go-node/pkg/cardano/types"
)

//CalculateHash hashes the values of utxos
func CalculateHash(txOut *types.UTXO) ([]byte, error) {

	marshal, err := json.Marshal(txOut)
	if err != nil {
		return nil, err
	}

	h := sha256.New()
	if _, err := h.Write(marshal); err != nil {
		return nil, err
	}

	return h.Sum(nil), nil
}
