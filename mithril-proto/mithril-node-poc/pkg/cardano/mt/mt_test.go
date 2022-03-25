package mt

import (
	"encoding/hex"
	"fmt"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"testing"
)

func TestRootCalc(t *testing.T) {
	mt := NewMerkleTree()
	mt.Add([]byte("Foo"))
	mt.Add([]byte("Bar"))
	mt.Add([]byte("Baz"))
	root, err := mt.GetRoot()
	require.NoError(t, err)
	expected, _ := hex.DecodeString("635ca493fe20a7b8485d2e4c650e33444664b4ce0773c36d2a9da79176f6889c")
	assert.Equal(t, expected, root)
}

func TestProof(t *testing.T) {
	mt := NewMerkleTree()
	mt.Add([]byte("Foo"))
	mt.Add([]byte("Bar"))
	mt.Add([]byte("Baz"))
	mt.Add([]byte("Quux"))
	root, err := mt.GetRoot()
	require.NoError(t, err)
	fmt.Printf("%+v\n", root)

	proof, err := mt.GetProof([]byte("Baz"))
	require.NoError(t, err)
	fmt.Printf("%+v\n", proof)

	mt2 := NewMerkleTree()
	valid, err := mt2.VerifyProof([]byte("Baz"), proof, root)
	require.NoError(t, err)
	assert.True(t, valid)
}
