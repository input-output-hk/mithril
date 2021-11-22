package types

import (
	"encoding/hex"
	"encoding/json"
)

//type (
//	Output struct {
//		Index   int64
//		Address string
//		Value   int
//	}
//
//	UTxO struct {
//		Hash    string
//		TxId    string
//		Outputs []Output
//	}
//
//	UTXOSet struct {
//		BlockNumber uint64
//		BlockHash   string
//		UTxOs       []UTxO
//	}
//)

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
