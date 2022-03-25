package node

import (
	"github.com/input-output-hk/mithril/mithril-proto/mithril-node-poc/pkg/cert"
	"github.com/input-output-hk/mithril/mithril-proto/mithril-node-poc/pkg/mithril"
	"github.com/libp2p/go-libp2p-core/peer"
)

type sigProcess struct {
	params       mithril.Parameters
	participants []*mithril.Participant
	hashStr      string
	cert         cert.Certificate
	signatures   map[peer.ID][]Signature
}
