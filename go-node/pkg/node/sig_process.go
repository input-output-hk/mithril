package node

import (
	"github.com/input-output-hk/mithril/go-node/pkg/cert"
	"github.com/input-output-hk/mithril/go-node/pkg/mithril"
	"github.com/libp2p/go-libp2p-core/peer"
)

type sigProcess struct {
	params       mithril.Parameters
	participants []mithril.Participant
	hashStr      string
	cert         cert.Certificate
	signatures   map[peer.ID][]Signature
}
