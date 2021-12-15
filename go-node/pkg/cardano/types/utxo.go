package types

import (
	"encoding/hex"
	"encoding/json"
)

// Address is an alias for string that represents address field
type Address string

type Hash []byte

func (h Hash) MarshalJSON() ([]byte, error) {
	return json.Marshal(hex.EncodeToString(h))
}

func (h *Hash) UnmarshalJSON(src []byte) error {
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

// UTXO is basic type that represents unspent tx output
type UTXO struct {
	Address Address `json:"address"`
	Hash    Hash    `json:"hash"`
	Index   int64   `json:"index"`
	Value   int64   `json:"value"`
}
