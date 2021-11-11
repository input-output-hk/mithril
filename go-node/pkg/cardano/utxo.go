package cardano

type (
	Output struct {
		Index int64
		Address string
		Value   int
	}

	UTxO struct {
		Hash    string
		TxId    string
		Outputs []Output
	}

	UTxOSet struct {
		BlockNumber uint64
		BlockHash   string
		UTxOs        []UTxO
	}
)

//