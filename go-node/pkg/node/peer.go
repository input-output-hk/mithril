package node

import (
	"context"
	"encoding/binary"
	"fmt"
	"github.com/libp2p/go-libp2p-core/network"
	"github.com/libp2p/go-libp2p-core/peer"
	"github.com/multiformats/go-multiaddr"
	"time"
)

const (
	Initial int = iota
	Connected
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
}

func (p PeerNode) Close() error {
	p.ctxCancel()

	if err := p.stream.Close(); err != nil {
		return err
	}

	return nil
}

func (p PeerNode) readStream() {

	for {
		select {
		case <-p.ctx.Done():
			return

		default:
			var counter uint64
			// get Message
			// h map

			err := binary.Read(p.stream, binary.BigEndian, &counter)
			if err != nil {
				fmt.Printf("Error reading from %s: %+v\n", p.stream.ID(), err)
				return
			}

			fmt.Printf("Received %d from %s\n", counter, p.stream.ID())
		}

	}
}

func (p PeerNode) writeStream() {
	var counter uint64

	for {
		select {
		case <-p.ctx.Done():
			return

		case <-time.After(time.Second):
			counter++

			err := binary.Write(p.stream, binary.BigEndian, counter)
			if err != nil {
				p.node.HandlePeerTimeout(p)
				return
			}
		}
	}
}

func (p PeerNode) AddrInfo() peer.AddrInfo {
	return peer.AddrInfo{
		ID:    p.stream.Conn().RemotePeer(),
		Addrs: []multiaddr.Multiaddr{p.stream.Conn().RemoteMultiaddr()},
	}
}
