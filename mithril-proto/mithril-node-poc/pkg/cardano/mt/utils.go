package mt

import (
	"crypto"
	"encoding/json"
	"github.com/input-output-hk/mithril/mithril-proto/mithril-node-poc/pkg/cardano/types"
)

//CalculateHash hashes the values of utxos
func CalculateHash(txOut *types.UTXO) ([]byte, error) {

	marshal, err := json.Marshal(txOut)
	if err != nil {
		return nil, err
	}

	h := crypto.BLAKE2b_256.New()
	if _, err := h.Write(marshal); err != nil {
		return nil, err
	}

	return h.Sum(nil), nil
}
