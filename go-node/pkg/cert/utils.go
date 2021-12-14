package cert

import "crypto/sha256"

func Hash(c Certificate) []byte {
	h := sha256.New()
	h.Write(c.MerkleRoot)
	return h.Sum(nil)
}
