package mt

type MerkleTree interface {
	Add(byte []byte) error
	GetRoot() ([]byte, error)
}
