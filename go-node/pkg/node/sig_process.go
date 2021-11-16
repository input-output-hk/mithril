package node

import (
	"github.com/input-output-hk/mithril/go-node/pkg/cert"
	"github.com/input-output-hk/mithril/go-node/pkg/mithril"
	"github.com/libp2p/go-libp2p-core/peer"
)

type sigProcess struct {
	params       mithril.Parameters
	participants []mithril.Participant
	cert         cert.Certificate
	pLen         int
	signatures   map[peer.ID][]Signature
}
