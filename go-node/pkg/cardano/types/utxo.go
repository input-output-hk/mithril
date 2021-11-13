package types

type (
	Output struct {
		Index   int64
		Address string
		Value   int
	}

	UTxO struct {
		Hash    string
		TxId    string
		Outputs []Output
	}

	UTXOSet struct {
		BlockNumber uint64
		BlockHash   string
		UTxOs       []UTxO
	}
)

// Address is an alias for string that represents address field
type Address string

// TxOut is basic type that represents tx output
type TxOut struct {
	Address Address
	TxID    int64
	Index   int64
	Value   int64
}
