package cardano

import (
	"context"
	"encoding/base64"
	"github.com/input-output-hk/mithril/go-node/internal/log"
	"github.com/input-output-hk/mithril/go-node/pkg/cardano/mt"
	"github.com/jackc/pgx/v4"
)

func ProcessUTXO(storage Storage, tree mt.MerkleTree, blockNumber uint64) (root []byte, err error) {
	err = storage.Transact(context.TODO(), func(tx pgx.Tx) error {
		txOutLen, err := storage.UTXORepository.GetTxOutputs(tx, blockNumber, tree)
		//
		//// Calculate hashes
		////for address, utxos := range txOuts {
		//for _, utxos := range txOuts {
		//	for _, utxo := range utxos {
		//		var err error
		//		hash, err := mt.CalculateHash(utxo)
		//		if err != nil {
		//			return err
		//		}
		//
		//		tree.Add(hash)
		//	}
		//}
		//
		//for _, tx := range txOuts {
		//	fmt.Println(tx)
		//}
		//
		root, err = tree.GetRoot()

		log.Infow("Calculating UTxO MT root hash",
			"block_number", blockNumber,
			"tx_len", txOutLen,
			"hash", base64.StdEncoding.EncodeToString(root),
		)

		return err
	})

	return root, err
}
