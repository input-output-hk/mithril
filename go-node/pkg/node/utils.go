package node

import (
	"encoding/base64"
	"github.com/input-output-hk/mithril/go-node/pkg/cardano"
	"github.com/input-output-hk/mithril/go-node/pkg/cardano/mt"
)

func GetBlockMTHash(s cardano.Storage, blockNumber uint64) (string, error) {
	tree := mt.NewMerkleTree()
	hash, err := cardano.ProcessUTXO(s, tree, blockNumber)
	if err != nil {
		return "", err
	}
	return base64.StdEncoding.EncodeToString(hash), nil
}
