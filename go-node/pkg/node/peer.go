package node

import (
	"bufio"
	"context"
	"encoding/json"
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
		rw:         bufio.NewReadWriter(bufio.NewReader(stream), bufio.NewWriter(stream)),
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

	rw      *bufio.ReadWriter
	writeCh chan Message
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
		str, err := p.rw.ReadString('\n')
		if err != nil {
			panic(err)
		}

		fmt.Println(str)
	}
}

func (p PeerNode) writeStream() {
	for {
		select {
		case <-p.ctx.Done():
			return

		case m := <-p.writeCh:
			data, _ := json.Marshal(m)
			p.rw.WriteString(fmt.Sprintf("%s\n", string(data)))
			p.rw.Flush()
		}
	}
}

func (p PeerNode) AddrInfo() peer.AddrInfo {
	return peer.AddrInfo{
		ID:    p.stream.Conn().RemotePeer(),
		Addrs: []multiaddr.Multiaddr{p.stream.Conn().RemoteMultiaddr()},
	}
}