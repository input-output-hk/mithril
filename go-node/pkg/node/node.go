package node

import (
	"container/list"
	"context"
	"fmt"
	"github.com/input-output-hk/mithril/go-node/pkg/config"
	"github.com/libp2p/go-libp2p"
	"github.com/libp2p/go-libp2p-core/host"
	"github.com/libp2p/go-libp2p-core/network"
	"github.com/libp2p/go-libp2p-core/peer"
	noise "github.com/libp2p/go-libp2p-noise"
	"github.com/libp2p/go-libp2p/p2p/discovery/mdns"
	"os"
	"os/signal"
	"syscall"
)

func New(ctx context.Context, cfg *config.Config) (*Node, error) {

	node, err := libp2p.New(ctx,
		libp2p.ListenAddrStrings("/ip4/127.0.0.1/tcp/0"),
		libp2p.Security(noise.ID, noise.New),
	)
	if err != nil {
		return nil, err
	}

	return &Node{
		ctx:    ctx,
		host:   node,
		config: cfg,
	}, nil
}

type Node struct {
	ctx    context.Context
	config *config.Config
	self   *PeerNode
	host   host.Host
	peers  list.List
}

func (n *Node) ServeNode() error {

	// Print this node's addresses and ID
	fmt.Println("Addresses:", n.host.Addrs())
	fmt.Println("ID:", n.host.ID())
	for _, multiAddr := range n.host.Addrs() {
		fmt.Println("Full Multi Addr: ", multiAddr.String()+"/p2p/"+n.host.ID().String())
	}

	// Setup peer discovery.
	discoveryService := mdns.NewMdnsService(n.host, discoveryNamespace)
	defer discoveryService.Close()

	discoveryService.RegisterNotifee(n)

	// This gets called every time a peer connects and opens a stream to this node.
	n.host.SetStreamHandler(protocolID, func(s network.Stream) {
		n.self = newPeer(n.ctx, *n, s)
	})

	sigCh := make(chan os.Signal)
	signal.Notify(sigCh, syscall.SIGKILL, syscall.SIGINT)

	<-sigCh
	return nil
}

func (n Node) HandlePeerFound(peerAddrInfo peer.AddrInfo) {
	if peerAddrInfo.ID == n.host.ID() {
		return
	}

	if err := n.host.Connect(n.ctx, peerAddrInfo); err != nil {
		panic(err)
	}

	stream, err := n.host.NewStream(n.ctx, peerAddrInfo.ID, protocolID)
	if err != nil {
		panic(err)
	}

	p := newPeer(n.ctx, n, stream)
	n.SayHello(p)

	fmt.Println("HandlePeerFound", peerAddrInfo)
}

func (n Node) HandlePeerTimeout(peerNode PeerNode) {
	peerNode.Close()
}

func (n Node) SayHello(peerNode *PeerNode) {
	m := Message{
		Type: helloMessage,
		Payload: Hello{
			Text: "Keys will be provided",
		},
	}

	peerNode.writeCh <- m
}