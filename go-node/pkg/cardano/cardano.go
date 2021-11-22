package cardano

import (
	"context"
	"encoding/hex"
	"github.com/input-output-hk/mithril/go-node/internal/log"
	"github.com/input-output-hk/mithril/go-node/pkg/cardano/mt"
	"github.com/jackc/pgx/v4"
)

func ProcessUTXO(ctx context.Context, tx pgx.Tx, tree mt.MerkleTree, blockNumber uint64) ([]byte, []byte, error) {

	outputs, err := GetAllTxOutputs(ctx, tx, blockNumber)
	if err != nil {
		return nil, nil, err
	}

	blockHash, err := GetBlockHash(tx, blockNumber)
	if err != nil {
		return nil, nil, err
	}

	for _, txOut := range outputs {
		hash, err := mt.CalculateHash(txOut)
		if err != nil {
			return nil, nil, err
		}

		err = tree.Add(hash)
		if err != nil {
			return nil, nil, err
		}
	}

	root, err := tree.GetRoot()
	if err != nil {
		return nil, nil, err
	}

	log.Infow("Calculating UTxO MT root hash",
		"block_number", blockNumber,
		"tx_len", len(outputs),
		"block_hash", hex.EncodeToString(blockHash),
		"merkle_root", hex.EncodeToString(root),
	)

	return root, blockHash, err
}
