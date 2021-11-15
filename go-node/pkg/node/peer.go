package node

import (
	"context"
	"encoding/json"
	"fmt"
	"github.com/input-output-hk/mithril/go-node/pkg/mithril"
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

func newPeer(ctx context.Context, node Node, stream network.Stream) *PeerNode {
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
	node       Node
	stream     network.Stream
	foundAt    time.Time
	lastSeenAt time.Time
	status     int
	writeCh    chan Message

	participant mithril.Participant
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

func (p PeerNode) readStream() {
	decoder := json.NewDecoder(p.stream)

	for {
		fmt.Printf("%s:%d:%d: Waiting input messages\n", p.Id(), p.node.participant.PartyId, p.participant.PartyId)
		fmt.Println(p.participant)

		var m Message
		if err := decoder.Decode(&m); err != nil {
			panic(err)
		}

		switch m.Type {
		case helloMessage:
			var hello Hello
			if err := readMessage(m.Payload, &hello); err != nil {
				panic(err)
			}
			p.OnHello(hello)


		default:
			fmt.Println("Unknown message:", m.Payload)
		}
	}
}

func (p PeerNode) writeStream() {
	for {
		select {
		case <-p.ctx.Done():
			return

		case m := <-p.writeCh:
			fmt.Printf("Sending message: %d --> %d: %s\n", p.node.participant.PartyId, p.participant.PartyId, m.Type)
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
	p.participant = mithril.NewParticipant(hello.PartyId, hello.Stake, hello.PublicKey)
	p.status = Ready
	fmt.Printf("On node %d peer %d is ready\n", p.node.participant.PartyId, p.participant.PartyId)
}

func (p *PeerNode) OnSigRequest(sigReq SigRequest) {
	p.node.HandleSigRequest(p, sigReq)
}