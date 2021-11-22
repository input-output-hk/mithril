package node

import (
	"container/list"
	"context"
	"encoding/base64"
	"github.com/input-output-hk/mithril/go-node/internal/log"
	"github.com/input-output-hk/mithril/go-node/internal/pg"
	"github.com/input-output-hk/mithril/go-node/pkg/cert"
	"github.com/input-output-hk/mithril/go-node/pkg/config"
	"github.com/input-output-hk/mithril/go-node/pkg/mithril"
	"github.com/jackc/pgx/v4"
	"github.com/libp2p/go-libp2p"
	"github.com/libp2p/go-libp2p-core/host"
	"github.com/libp2p/go-libp2p-core/network"
	"github.com/libp2p/go-libp2p-core/peer"
	noise "github.com/libp2p/go-libp2p-noise"
	"github.com/libp2p/go-libp2p/p2p/discovery/mdns"
	"github.com/pkg/errors"
	"strings"
	"sync"
	"time"
)

const (
	blockNumber   = 160000
	masterPartyId = 1
)

func New(ctx context.Context, cfg *config.Config, conn *pgx.Conn) (*Node, error) {

	node, err := libp2p.New(ctx,
		libp2p.ListenAddrStrings("/ip4/127.0.0.1/tcp/0"),
		libp2p.Security(noise.ID, noise.New),
	)
	if err != nil {
		return nil, err
	}

	mcfg := cfg.Mithril
	part := mcfg.Participants[mcfg.PartyId]

	initializer := mithril.DecodeInitializer(part.Initializer)

	return &Node{
		ctx:       ctx,
		host:      node,
		config:    cfg,
		peerNodes: make(map[peer.ID]*PeerNode, 0),

		nextBlockNumber: blockNumber,
		conn:            conn,

		// mithril
		initializer: initializer,
		participant: initializer.Participant(),
	}, nil
}

type Node struct {
	ctx      context.Context
	config   *config.Config
	host     host.Host
	peers    list.List
	peerLock sync.Mutex
	sigLock  sync.Mutex
	stream   network.Stream

	conn      *pgx.Conn
	peerNodes map[peer.ID]*PeerNode

	nextBlockNumber uint64
	sigProcess      *sigProcess

	initializer mithril.Initializer
	participant mithril.Participant
}

func (n *Node) ServeNode() error {

	var multiAddrs []string
	for _, multiAddr := range n.host.Addrs() {
		multiAddrs = append(multiAddrs, multiAddr.String()+"/p2p/"+n.host.ID().String())
	}

	log.Infow("Starting Mithril Node",
		"node_id", n.host.ID(),
		"is_leader", n.config.Leader,
		"cfg_slot", n.config.Mithril.PartyId,
		"party_id", n.participant.PartyId,
		"stake", n.participant.Stake,
		"addresses", n.host.Addrs(),
		"full_multi_addrs", multiAddrs,
		"params", n.config.Mithril.Params,
	)

	// Setup peer discovery.
	discoveryService := mdns.NewMdnsService(n.host, discoveryNamespace)
	defer discoveryService.Close()

	discoveryService.RegisterNotifee(n)

	// This gets called every time a peer connects and opens a stream to this node.
	n.host.SetStreamHandler(protocolID, func(s network.Stream) {
		n.HandleNewPeer(s)
	})

	return n.mainLoop()
}

func (n *Node) Shutdown() error {
	n.peerLock.Lock()
	defer n.peerLock.Unlock()

	for _, p := range n.peerNodes {
		_ = p.Close()
	}

	n.host.Close()
	n.ctx.Done()
	return nil
}

func (n *Node) mainLoop() error {
	time.Sleep(5 * time.Second)

	ticker := time.NewTicker(30 * time.Second)

	for {
		select {
		case <-n.ctx.Done():
			return n.Shutdown()

		case <-ticker.C:
			if n.config.Leader && len(n.peerNodes) > 0 {
				n.CreateSigRequest()
			}
		}
	}
}

func (n *Node) HandleNewPeer(stream network.Stream) {
	n.peerLock.Lock()
	defer n.peerLock.Unlock()

	peerId := stream.Conn().RemotePeer()
	if _, ok := n.peerNodes[peerId]; ok {
		_ = stream.Close()
		return
	}

	p := newPeer(n.ctx, n, stream)
	n.peerNodes[p.Id()] = p
	n.GreetNewPeer(p)

	log.Infow("New peer connection",
		"peer_id", p.Id(),
		"stream_id", stream.ID(),
	)
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

	n.HandleNewPeer(stream)
}

func (n *Node) HandlePeerLost(peerNode *PeerNode) {
	n.peerLock.Lock()
	defer n.peerLock.Unlock()

	delete(n.peerNodes, peerNode.Id())
	_ = peerNode.Close()
}

func (n *Node) GreetNewPeer(peerNode *PeerNode) {
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

func (n *Node) SendHelloMessage(peer *PeerNode) {
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

func (n *Node) CreateSigRequest() {
	n.sigLock.Lock()
	defer n.sigLock.Unlock()

	defer func() {
		n.nextBlockNumber += 10000
	}()

	hash, blockHash, err := GetBlockMTHash(n.conn, n.nextBlockNumber)
	if err != nil {
		log.Error(err)
		return
	}

	mcfg := n.config.Mithril
	participants := []mithril.Participant{n.participant}

	var peerNodes []*PeerNode
	for _, p := range n.peerNodes {
		if p.status != Ready {
			continue
		}
		participants = append(participants, p.participant)
		peerNodes = append(peerNodes, p)
	}

	params := mithril.Parameters{K: mcfg.Params.K, M: mcfg.Params.M, PhiF: mcfg.Params.PhiF}

	n.sigProcess = &sigProcess{
		participants: participants,
		hashStr:      base64.StdEncoding.EncodeToString(hash),
		cert: cert.Certificate{
			Id:           uint64(time.Now().Unix()),
			Params:       params,
			BlockNumber:  n.nextBlockNumber,
			BlockHash:    blockHash,
			MerkleRoot:   hash,
			SigStartedAt: time.Now(),
		},
		signatures: make(map[peer.ID][]Signature, 0),
	}

	req := SigRequest{
		RequestId:    n.sigProcess.cert.Id,
		Params:       params,
		Participants: participants,
		BlockNumber:  n.nextBlockNumber,
		BlockHash:    "<block hash>",
		MerkleRoot:   n.sigProcess.hashStr,
	}

	msg := Message{
		Type:    sigRequest,
		Payload: req,
	}

	log.Infow("Issued multiSig request",
		"block_number", n.nextBlockNumber,
		"hash", hash,
		"params", params,
		"participants_amount", len(participants),
	)

	for _, p := range peerNodes {
		p.writeCh <- msg
	}

	sigs, err := n.CreateNodeSignatures(req, participants)
	if err != nil {
		return
	}

	n.sigProcess.signatures[n.host.ID()] = sigs
}

func (n *Node) CreateNodeSignatures(req SigRequest, participants []mithril.Participant) ([]Signature, error) {

	var success uint64 = 0
	indices := make([]uint64, req.Params.K)
	signer := mithril.NewSigner(n.initializer, participants)

	var i uint64
	for i = 0; i < req.Params.M && success < req.Params.K; i++ {
		if signer.EligibilityCheck(i, req.MerkleRoot) {
			indices[success] = i
			success++
		}
	}

	if success < req.Params.K {
		return nil, errors.New("not enough indices for the sig")
	}

	var signatures []Signature
	for i = 0; i < req.Params.K; i++ {
		sign, err := signer.Sign(indices[i], req.MerkleRoot)
		if err != nil {
			return nil, err
		}

		signatures = append(signatures, Signature{
			Index: sign.Index(),
			Sig:   sign.Base64(),
		})
	}
	return signatures, nil
}

func (n *Node) HandleSigRequest(peer *PeerNode, req SigRequest) {
	log.Infow("Received sig request",
		"params", req.Params,
		"block_number", req.BlockNumber,
		"block_hash", req.BlockHash,
		"merkle_root", req.MerkleRoot,
		"party_id", n.participant.PartyId,
		"stake", n.participant.Stake,
	)

	hash, _, err := GetBlockMTHash(n.conn, req.BlockNumber)
	if err != nil {
		log.Error(err)
		return
	}

	hashStr := base64.StdEncoding.EncodeToString(hash)
	if strings.Compare(hashStr, req.MerkleRoot) != 0 {
		log.Infow("MT hash didn't match",
			"req_merkle_root", req.MerkleRoot,
			"hash", hash,
		)
		return
	}

	var participants []mithril.Participant
	for _, p := range req.Participants {
		participants = append(participants, mithril.NewParticipant(p.PartyId, p.Stake, p.PublicKey))
	}

	sigs, err := n.CreateNodeSignatures(req, participants)
	if err != nil {
		log.Errorw("Failed to create signatures",
			"err", err,
		)
		return
	}

	response := Message{
		Type: sigResponse,
		Payload: SigResponse{
			RequestId:  req.RequestId,
			Signatures: sigs,
		},
	}

	peer.writeCh <- response

	log.Infow("Signed request",
		"request_id", req.RequestId,
		"block_number", req.BlockNumber,
		"hash", req.MerkleRoot,
	)
}

func (n *Node) HandleSigResponse(peer *PeerNode, res SigResponse) {
	n.sigLock.Lock()
	defer n.sigLock.Unlock()

	log.Infow("Received sig response",
		"peer_id", peer.Id(),
		"request_id", res.RequestId,
		"signature_amount", len(res.Signatures),
	)

	if n.sigProcess == nil || n.sigProcess.cert.Id != res.RequestId {
		log.Infow("Sig timout")
		return
	}

	signer := mithril.NewSigner(n.initializer, n.sigProcess.participants)
	clerk := signer.Clerk()
	defer clerk.Free()

	for _, s := range res.Signatures {
		sig := mithril.DecodeSignature(s.Sig, s.Index)
		err := clerk.VerifySign(n.sigProcess.hashStr, s.Index, sig)
		if err != nil {
			log.Errorw("Failed to verify peer sign",
				"peer_id", peer.Id(),
				"index", s.Index,
				"err", err,
			)
			return
		}
	}

	n.sigProcess.signatures[peer.Id()] = res.Signatures
	n.TryAggregate(clerk)
}

func (n *Node) TryAggregate(clerk mithril.Clerk) {
	var sigs []*mithril.Signature
	for _, sa := range n.sigProcess.signatures {
		for _, s := range sa {
			sig := mithril.DecodeSignature(s.Sig, s.Index)
			sigs = append(sigs, sig)
		}
	}

	defer func() {
		for _, s := range sigs {
			s.Free()
		}
	}()

	multiSig, err := clerk.Aggregate(sigs, n.sigProcess.hashStr)
	if err != nil {
		if err == mithril.ErrNotEnoughSignatures {
			return
		}

		log.Errorw("Failed to aggregate",
			"request_id", n.sigProcess.cert.Id,
		)
		n.sigProcess = nil
		return
	}

	err = clerk.VerifyMultiSign(multiSig, n.sigProcess.hashStr)
	if err != nil {
		log.Errorw("Failed to validate multiSig")
		return
	}

	n.sigProcess.cert.MultiSig = multiSig.Encode()

	defer func() {
		n.sigProcess = nil
	}()

	err = pg.WithTX(n.ctx, n.conn, func(ctx context.Context, tx pgx.Tx) error {
		c := n.sigProcess.cert
		c.SigFinishedAt = time.Now()
		return cert.Save(ctx, tx, &c)
	})

	if err != nil {
		log.Errorw("Failed to save cert to DB", "err", err)
		return
	}

	log.Infow("MultiSig has been aggregated")
}
