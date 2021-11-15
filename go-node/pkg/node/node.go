package node

import (
	"bufio"
	"container/list"
	"context"
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

const masterPartyId = 1

func New(ctx context.Context, cfg *config.Config) (*Node, error) {

	node, err := libp2p.New(ctx,
		libp2p.ListenAddrStrings("/ip4/127.0.0.1/tcp/0"),
		libp2p.Security(noise.ID, noise.New),
	)
	if err != nil {
		return nil, err
	}

	mcfg := cfg.Mithril
	part := mcfg.Participants[mcfg.PartyId]

	params := mithril.Parameters{K: mcfg.Params.K, M: mcfg.Params.M, PhiF: mcfg.Params.PhiF}
	initializer := mithril.NewInitializer(params, part.PartyId, part.Stake)
	fmt.Println("PartyId", part)

	return &Node{
		ctx:         ctx,
		host:        node,
		config:      cfg,
		participant: initializer.Participant(),
		peerNodes:   make(map[peer.ID]*PeerNode, 0),
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
	peerNodes   map[peer.ID]*PeerNode
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
		fmt.Println("SetStreamHandler:", s.Conn().RemotePeer().String())
		p := newPeer(n.ctx, *n, s)
		n.GreetNewPeer(p)
		n.peerNodes[p.Id()] = p
	})

	if n.participant.PartyId == masterPartyId {
		fmt.Println("Doing master stream...")
		go n.HandleMasterTask()
	}

	sigCh := make(chan os.Signal)
	signal.Notify(sigCh, syscall.SIGKILL, syscall.SIGINT)

	<-sigCh
	return nil
}

func (n Node) HandleMasterTask() {
	timer := time.NewTicker(3 * time.Second)

	for {
		select {
		case <-timer.C:
			msg := Message{
				Type:    "123",
				Payload: nil,
			}

			fmt.Println("Task...", len(n.peerNodes))
			for _, p := range n.peerNodes {
				p.writeCh <- msg
			}
		}
	}
}

func (n *Node) HandlePeerFound(peerAddrInfo peer.AddrInfo) {
	if peerAddrInfo.ID == n.host.ID() {
		return
	}

	if n.participant.PartyId == masterPartyId {
		return
	}

	if err := n.host.Connect(n.ctx, peerAddrInfo); err != nil {
		panic(err)
	}

	stream, err := n.host.NewStream(n.ctx, peerAddrInfo.ID, protocolID)
	if err != nil {
		panic(err)
	}

	fmt.Println("HandlePeerFound:", stream.Conn().RemotePeer().String())
	p := newPeer(n.ctx, *n, stream)
	n.peerNodes[p.Id()] = p
}

func (n Node) HandlePeerTimeout(peerNode PeerNode) {
	peerNode.Close()
}

func (n Node) GreetNewPeer(peerNode *PeerNode) {
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
			CardanoAddress: "<CADDR>",
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

	peer.writeCh <- response
}
