package node

import (
	"context"
	"encoding/json"
	"github.com/input-output-hk/mithril/mithril-proto/mithril-node-poc/internal/log"
	"github.com/input-output-hk/mithril/mithril-proto/mithril-node-poc/pkg/mithril"
	"github.com/libp2p/go-libp2p-core/network"
	"github.com/libp2p/go-libp2p-core/peer"
	"github.com/multiformats/go-multiaddr"
	"time"
)

const (
	Initial int = iota
	Ready
	Zombie
)

func newPeer(ctx context.Context, node *Node, stream network.Stream) *PeerNode {
	peerCtx, cancel := context.WithCancel(ctx)

	pn := &PeerNode{
		ctx:        peerCtx,
		ctxCancel:  cancel,
		node:       node,
		stream:     stream,
		foundAt:    time.Now(),
		lastSeenAt: time.Now(),
		status:     Initial,
		writeCh:    make(chan Message, 32),
	}

	go pn.readStream()
	go pn.writeStream()

	return pn
}

type PeerNode struct {
	ctx        context.Context
	ctxCancel  context.CancelFunc
	node       *Node
	stream     network.Stream
	foundAt    time.Time
	lastSeenAt time.Time
	status     int
	writeCh    chan Message

	participant *mithril.Participant
}

func (p PeerNode) Id() peer.ID {
	return p.stream.Conn().RemotePeer()
}

func (p PeerNode) Close() error {
	p.ctxCancel()

	if err := p.stream.Close(); err != nil {
		return err
	}

	return nil
}

func (p *PeerNode) readStream() {
	defer p.node.HandlePeerLost(p)

	decoder := json.NewDecoder(p.stream)

	for {
		var m Message
		if err := decoder.Decode(&m); err != nil {
			log.Errorw("Failed to read peer",
				"peer_id", p.Id(),
				"stream_id", p.stream.ID(),
				"err", err,
			)
			return
		}

		switch m.Type {
		case helloMessage:
			var hello Hello
			err := readMessage(p, m, &hello)
			if err != nil {
				continue
			}
			p.OnHello(hello)

		case sigRequest:
			var sigReq SigRequest
			err := readMessage(p, m, &sigReq)
			if err != nil {
				continue
			}

			in := m.Payload.(map[string]interface{})
			buf, err := json.Marshal(in["certificate"])

			if err != nil {
				continue
			}

			if err := json.Unmarshal(buf, &sigReq.Cert); err != nil {
				log.Error(err)
			}

			p.OnSigRequest(sigReq)

		case sigResponse:
			var sigRes SigResponse
			err := readMessage(p, m, &sigRes)
			if err != nil {
				continue
			}
			p.OnSigResponse(sigRes)

		default:
			log.Error("Unknown message type",
				"peer_id", p.Id(),
				"msg_type", m.Type,
				"payload", m.Payload,
			)
		}
	}
}

func (p *PeerNode) writeStream() {
	defer p.node.HandlePeerLost(p)

	for {
		select {
		case <-p.ctx.Done():
			return

		case m := <-p.writeCh:
			data, _ := json.Marshal(m)
			p.stream.Write(data)
		}
	}
}

func (p PeerNode) AddrInfo() peer.AddrInfo {
	return peer.AddrInfo{
		ID:    p.stream.Conn().RemotePeer(),
		Addrs: []multiaddr.Multiaddr{p.stream.Conn().RemoteMultiaddr()},
	}
}

func (p *PeerNode) OnHello(hello Hello) {
	var err error
	p.participant, err = mithril.NewParticipant(hello.PartyId, hello.Stake, hello.PublicKey)
	if err != nil {
		return
	}

	p.status = Ready

	log.Infow("Peer introduction",
		"peer_id", p.Id().String(),
		"stream_id", p.stream.ID(),
		"party_id", p.participant.PartyId,
		"stake", p.participant.Stake,
	)
}

func (p *PeerNode) OnSigRequest(sigReq SigRequest) {
	err := p.node.HandleSigRequest(p, sigReq)
	if err != nil {
		log.Error(err)
	}
}

func (p *PeerNode) OnSigResponse(sigRes SigResponse) {
	err := p.node.HandleSigResponse(p, sigRes)
	if err != nil {
		log.Error(err)
	}
}
