package node

import (
	"bufio"
	"container/list"
	"context"
	"encoding/json"
	"fmt"
	"github.com/input-output-hk/mithril/go-node/pkg/config"
	"github.com/input-output-hk/mithril/go-node/pkg/mithril"
	"github.com/libp2p/go-libp2p"
	"github.com/libp2p/go-libp2p-core/host"
	"github.com/libp2p/go-libp2p-core/network"
	"github.com/libp2p/go-libp2p-core/peer"
	noise "github.com/libp2p/go-libp2p-noise"
	"github.com/libp2p/go-libp2p/p2p/discovery/mdns"
	"os"
	"os/signal"
	"syscall"
	"time"
)

func New(ctx context.Context, cfg *config.Config) (*Node, error) {

	node, err := libp2p.New(ctx,
		libp2p.ListenAddrStrings("/ip4/127.0.0.1/tcp/0"),
		libp2p.Security(noise.ID, noise.New),
	)
	if err != nil {
		return nil, err
	}

	p := mithril.Parameters{K: cfg.Mithril.Params.K, M: cfg.Mithril.Params.M, PhiF: cfg.Mithril.Params.PhiF}
	i := mithril.NewInitializer(p, 1, 1)

	return &Node{
		ctx:         ctx,
		host:        node,
		config:      cfg,
		participant: i.Participant(),
	}, nil
}

type Node struct {
	ctx    context.Context
	config *config.Config
	host   host.Host
	peers  list.List

	stream network.Stream
	rw     *bufio.ReadWriter

	participant mithril.Participant
	peerNodes   []*PeerNode
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
		go n.readStream(s)
		go n.writeStream(s)
	})

	sigCh := make(chan os.Signal)
	signal.Notify(sigCh, syscall.SIGKILL, syscall.SIGINT)

	<-sigCh
	return nil
}

func (n *Node) HandlePeerFound(peerAddrInfo peer.AddrInfo) {
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

	p := newPeer(n.ctx, *n, stream)

	n.SendHelloMessage(p)
	n.peerNodes = append(n.peerNodes, p)
}

func (n Node) HandlePeerTimeout(peerNode PeerNode) {
	peerNode.Close()
}

func (n Node) SayHello(peerNode *PeerNode) {
	m := Message{
		Type: helloMessage,
		Payload: Hello{
			PartyId:   n.participant.PartyId,
			Stake:     n.participant.Stake,
			PublicKey: n.participant.PublicKey,
		},
	}
	peerNode.writeCh <- m
}

func (n Node) SendHelloMessage(peer *PeerNode) {
	msg := Message{
		Type: helloMessage,
		Payload: Hello{
			CardanoAddress: "",
			PartyId:        n.participant.PartyId,
			Stake:          n.participant.Stake,
			PublicKey:      n.participant.PublicKey,
		},
	}
	peer.writeCh <- msg
}

func (n Node) CreateSigRequest() {

	participants := []mithril.Participant{n.participant}

	for _, p := range n.peerNodes {
		participants = append(participants, p.participant)
	}

	msg := Message{
		Type: sigRequest,
		Payload: SigRequest{
			RequestId:    uint64(time.Now().Unix()),
			Params:       mithril.Parameters{},
			Participants: participants,
			BlockNumber:  3,
			BlockHash:    "<the hash>",
			MerkleRoot:   "123",
		},
	}

	for _, p := range n.peerNodes {
		p.writeCh <- msg

	}
}

func (n Node) HandleSigRequest(peer *PeerNode, req SigRequest) {
	initializer := mithril.NewInitializer(req.Params, n.participant.PartyId, n.participant.Stake)
	defer initializer.Free()

	signer := mithril.NewSigner(initializer, req.Participants)

	var success uint64 = 0
	indices := make([]uint64, req.Params.M)

	var i uint64
	for i = 0; i < req.Params.M && success < req.Params.K; i++ {
		if signer.EligibilityCheck(i, req.MerkleRoot) {
			indices[success] = i
			success++
		}
	}

	if success < req.Params.K {
		return
	}

	var signatures []Signature
	for i = 0; i < req.Params.K; i++ {
		sign, err := signer.Sign(indices[i], req.MerkleRoot)
		if err != nil {
			return
		}

		signatures = append(signatures, Signature{
			Index: sign.Index(),
			Sig:   sign.Base64(),
		})
	}

	response := Message{
		Type: sigResponse,
		Payload: SigResponse{
			RequestId:  req.RequestId,
			Signatures: signatures,
		},
	}

	fmt.Println("Sent Hello Message")
	peer.writeCh <- response
}

func (n Node) readStream(stream network.Stream) {
	decoder := json.NewDecoder(stream)

	for {
		var m Message
		if err := decoder.Decode(&m); err != nil {
			panic(err)
		}
		fmt.Println("Read", m)
	}
}

func (n Node) writeStream(stream network.Stream) {
}
