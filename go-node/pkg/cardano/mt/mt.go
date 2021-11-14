package mt

import (
	"github.com/wealdtech/go-merkletree"
	"github.com/wealdtech/go-merkletree/blake2b"
)

type MerkleTree interface {
	Add(data []byte) error
	GetRoot() ([]byte, error)
}

type MemMerkleTree struct {
	leaves [][]byte
	tree   *merkletree.MerkleTree
}

func NewMerkleTree() MerkleTree {
	return &MemMerkleTree{}
}

func (mt *MemMerkleTree) Add(data []byte) error {
	mt.leaves = append(mt.leaves, data)
	return nil
}

func (mt *MemMerkleTree) GetRoot() ([]byte, error) {
	tree, err := merkletree.NewUsing(mt.leaves, blake2b.New(), nil)

	if err != nil {
		return nil, err
	}

	mt.tree = tree

	return mt.tree.Root(), nil
}
