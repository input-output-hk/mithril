package cardano

import (
	"context"
	"encoding/hex"
	"fmt"
	"github.com/input-output-hk/mithril/go-node/internal/bm"
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

func ProcessTestUTXO(ctx context.Context, tx pgx.Tx, tree mt.MerkleTree, nodeId uint64, size uint64,
) ([]byte, []byte, []*bm.Event, error) {

	var events []*bm.Event

	fetchEvent := bm.New("Fetch UTXO set from DB")
	outputs, err := GetTxTestList(ctx, tx, size)
	if err != nil {
		return nil, nil, nil, err
	}
	fetchEvent.Stop()
	events = append(events, fetchEvent)

	blockHash, err := hex.DecodeString(fmt.Sprintf("000000000000000000%04d%010d", nodeId, size))
	if err != nil {
		return nil, nil, nil, err
	}

	buildMTEvent := bm.New("Build MerkleTree Event")
	for _, txOut := range outputs {
		hash, err := mt.CalculateHash(txOut)
		if err != nil {
			return nil, nil, nil, err
		}

		err = tree.Add(hash)
		if err != nil {
			return nil, nil, nil, err
		}
	}
	buildMTEvent.Stop()
	events = append(events, buildMTEvent)

	root, err := tree.GetRoot()
	if err != nil {
		return nil, nil, nil, err
	}

	log.Infow("Calculating TestRun UTxO MT root hash",
		"size", size,
		"tx_len", len(outputs),
		"block_hash", hex.EncodeToString(blockHash),
		"merkle_root", hex.EncodeToString(root),
	)

	return root, blockHash, events, err
}
