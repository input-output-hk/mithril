package mt

import (
	"encoding/hex"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"testing"
)

func TestNewStorage(t *testing.T) {
	mt := NewMerkleTree()
	mt.Add([]byte("Foo"))
	mt.Add([]byte("Bar"))
	mt.Add([]byte("Baz"))
	root, err := mt.GetRoot()
	require.Nil(t, err)
	expected, _ := hex.DecodeString("635ca493fe20a7b8485d2e4c650e33444664b4ce0773c36d2a9da79176f6889c")
	assert.Equal(t, expected, root)
}
