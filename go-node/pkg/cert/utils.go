package cert

import (
	"crypto/sha256"
	"strconv"
)

func Hash(c Certificate) []byte {
	buf := make([]byte, 0)

	buf = append(buf, []byte(c.MerkleRoot)...)
	buf = append(buf, []byte(c.PrevHash)...)

	for _, p := range c.Participants {
		buf = append(buf, []byte(strconv.FormatUint(p.PartyId, 10))...)
		buf = append(buf, []byte(strconv.FormatUint(p.Stake, 10))...)
	}

	h := sha256.New()
	h.Write(buf)
	return h.Sum(nil)
}

func GenesisHash() []byte {
	return make([]byte, 32, 32)
}
