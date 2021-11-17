package node

import (
	"github.com/input-output-hk/mithril/go-node/pkg/cardano"
	"github.com/input-output-hk/mithril/go-node/pkg/cardano/mt"
)

func GetBlockMTHash(s cardano.Storage, blockNumber uint64) ([]byte, error) {
	tree := mt.NewMerkleTree()
	return cardano.ProcessUTXO(s, tree, blockNumber)
}
