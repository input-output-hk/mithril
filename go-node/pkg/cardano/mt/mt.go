package mt

import (
	"github.com/wealdtech/go-merkletree"
	"github.com/wealdtech/go-merkletree/blake2b"
)

type MerkleTree interface {
	Add(data []byte) error
	GetRoot() ([]byte, error)
	GetProof(data []byte) (*Proof, error)
	VerifyProof(data []byte, proof *Proof, root []byte) (bool, error)
}

type Proof struct {
	Hashes [][]byte
	Index  uint64
}

type MemMerkleTree struct {
	leaves [][]byte
	tree   *merkletree.MerkleTree
}

// NewMerkleTree creates in-memory MerkleTree
func NewMerkleTree() MerkleTree {
	return &MemMerkleTree{}
}

// constructs actual merkle tree, computes all the hashes
func (mt *MemMerkleTree) buildTree() error {
	tree, err := merkletree.NewUsing(mt.leaves, blake2b.New(), nil)
	if err != nil {
		return err
	}
	mt.tree = tree
	return nil
}

// Add method adds a new leaf to the tree
func (mt *MemMerkleTree) Add(data []byte) error {
	// delete the tree if we had one already constructed
	mt.tree = nil
	mt.leaves = append(mt.leaves, data)
	return nil
}

// GetRoot returns the root of the MT
func (mt *MemMerkleTree) GetRoot() ([]byte, error) {
	if mt.tree == nil {
		err := mt.buildTree()
		if err != nil {
			return nil, err
		}
	}
	return mt.tree.Root(), nil
}

// GetProof returns a proof for the data in the tree if it exists, error otherwise
func (mt *MemMerkleTree) GetProof(data []byte) (*Proof, error) {
	if mt.tree == nil {
		err := mt.buildTree()
		if err != nil {
			return nil, err
		}
	}

	_proof, err := mt.tree.GenerateProof(data)
	if err != nil {
		return nil, err
	}

	proof := &Proof{
		Hashes: _proof.Hashes,
		Index:  _proof.Index,
	}

	return proof, nil
}

// VerifyProof verifies a proof of inclusion of a given data into the MerkleTree with the provided root
func (mt *MemMerkleTree) VerifyProof(data []byte, proof *Proof, root []byte) (bool, error) {
	_proof := &merkletree.Proof{
		Hashes: proof.Hashes,
		Index:  proof.Index,
	}

	return merkletree.VerifyProof(data, _proof, root)
}
