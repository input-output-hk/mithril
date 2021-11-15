package cardano

import (
	"context"
	"github.com/input-output-hk/mithril/go-node/pkg/cardano/mt"
	"github.com/input-output-hk/mithril/go-node/pkg/cardano/types"
	"github.com/jackc/pgx/v4"
)

func ProcessUTXO(storage Storage, tree mt.MerkleTree, blockNumber int64) (root []byte, err error) {
	// Get UTXO from DB
	var txOuts map[types.Address][]*types.UTXO

	err = storage.Transact(context.TODO(), func(tx pgx.Tx) error {
		txOuts, err = storage.UTXORepository.GetTxOutputs(tx, blockNumber)

		// Calculate hashes
		//for address, utxos := range txOuts {
		for _, utxos := range txOuts {
			for _, utxo := range utxos {
				var err error
				hash, err := mt.CalculateHash(utxo)
				if err != nil {
					return err
				}

				tree.Add(hash)
			}
		}

		root, err = tree.GetRoot()
		return err
	})

	return root, err
}


func GetLastBlockNumber() int64 {
	return 250000
}