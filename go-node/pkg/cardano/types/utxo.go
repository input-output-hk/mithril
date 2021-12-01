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

// UTXO is basic type that represents unspent tx output
type UTXO struct {
	Address Address `json:"address"`
	Hash    Hash    `json:"hash"`
	Index   int64   `json:"index"`
	Value   int64   `json:"value"`
}
