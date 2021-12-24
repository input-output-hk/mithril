package node

import (
	"bytes"
	"container/list"
	"context"
	"encoding/base64"
	"fmt"
	"github.com/input-output-hk/mithril/go-node/internal/bm"
	"sync"
	"time"

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
		nextTC:    make(chan bool, 10),
		host:      node,
		config:    cfg,
		peerNodes: make(map[peer.ID]*PeerNode, 0),

		nextBlockNumber: blockNumber,
		conn:            conn,

		Participant: initializer.Participant(),
	}, nil
}

type Node struct {
	ctx      context.Context
	nextTC   chan bool
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

	Participant mithril.Participant
}

func (n *Node) PartyId() uint64 {
	return n.Participant.PartyId
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
		"party_id", n.Participant.PartyId,
		"skate", n.Participant.Stake,
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

	if n.config.TestRun {
		return n.testRun()
	}
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

	ticker := time.NewTicker(30 * time.Second)

	c, err := cert.GetLastCert(n.ctx, n.conn, n.Participant.PartyId)
	if err != nil {
		return err
	}
	if c != nil {
		n.nextBlockNumber = c.BlockNumber + 10000
	}

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

func (n *Node) testRun() error {
	testSizes := []uint64{100000, 500000, 1000000, 5000000, 10000000}
	//testSizes := []uint64{10000000}
	nextIndex := 0

	n.nextTC <- true

	for {
		select {
		case <-n.ctx.Done():
			return n.Shutdown()

		case <-n.nextTC:
			if nextIndex < len(testSizes) {
				n.CreateTestSigRequest(testSizes[nextIndex])
				nextIndex++
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

	// No need to create a connection peer is a node itself.
	if peerAddrInfo.ID == n.host.ID() {
		return
	}

	if n.Participant.PartyId == masterPartyId {
		return
	}

	if err := n.host.Connect(n.ctx, peerAddrInfo); err != nil {
		log.Errorf("Failed to connect",
			"peer_addr", peerAddrInfo,
			"err", err,
		)
		return
	}

	stream, err := n.host.NewStream(n.ctx, peerAddrInfo.ID, protocolID)
	if err != nil {
		log.Errorf("Failed to create a stream",
			"peer_addr", peerAddrInfo,
			"err", err,
		)
		return
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
			PartyId:   n.Participant.PartyId,
			Stake:     n.Participant.Stake,
			PublicKey: n.Participant.PublicKey,
		},
	}
	peerNode.writeCh <- m
}

func (n *Node) CreateSigRequest() {
	n.sigLock.Lock()
	defer n.sigLock.Unlock()

	defer func() {
		n.nextBlockNumber += 10000
	}()

	lastCert, err := cert.GetLastCert(n.ctx, n.conn, n.PartyId())
	if err != nil {
		log.Error(err)
		return
	}

	var lastCertHash cert.Hex
	if lastCert != nil {
		lastCertHash = lastCert.CertHash
	} else {
		lastCertHash = cert.GenesisHash()
	}

	hash, blockHash, err := GetBlockMTHash(n.conn, n.nextBlockNumber)
	if err != nil {
		log.Error(err)
		return
	}

	mcfg := n.config.Mithril
	participants := []mithril.Participant{n.Participant}

	var peerNodes []*PeerNode
	for _, p := range n.peerNodes {
		if p.status != Ready {
			continue
		}
		participants = append(participants, p.participant)
		peerNodes = append(peerNodes, p)
	}

	nextCert := cert.Certificate{
		Id:           uint64(time.Now().Unix()),
		NodeId:       n.PartyId(),
		Participants: participants,
		PrevHash:     lastCertHash,
		BlockNumber:  n.nextBlockNumber,
		BlockHash:    blockHash,
		MerkleRoot:   hash,
		SigStartedAt: time.Now(),
	}
	nextCert.CertHash = cert.Hash(nextCert)

	params := mithril.Parameters{K: mcfg.Params.K, M: mcfg.Params.M, PhiF: mcfg.Params.PhiF}

	n.sigProcess = &sigProcess{
		participants: participants,
		hashStr:      base64.StdEncoding.EncodeToString(hash),
		cert:         nextCert,
		signatures:   make(map[peer.ID][]Signature, 0),
	}

	req := SigRequest{
		RequestId:    n.sigProcess.cert.Id,
		Params:       params,
		Participants: participants,
		Cert:         nextCert,
	}

	log.Infow("Issued multiSig request",
		"id", nextCert.Id,
		"cert_hash", nextCert.CertHash.String(),
		"prev_hash", nextCert.PrevHash.String(),
		"merle_root", nextCert.MerkleRoot.String(),
		"block_number", nextCert.BlockNumber,
		"block_hash", nextCert.BlockHash,
		"participants_amount", len(participants),
	)

	msg := Message{Type: sigRequest, Payload: req}
	for _, p := range peerNodes {
		p.writeCh <- msg
	}

	sigs, err := n.CreateNodeSignatures(req, participants)
	if err != nil {
		return
	}

	n.sigProcess.signatures[n.host.ID()] = sigs
}

func (n *Node) CreateTestSigRequest(size uint64) {
	n.sigLock.Lock()
	defer n.sigLock.Unlock()

	sigReqEvent := bm.New(fmt.Sprintf("Testing %d UTXO set", size))

	lastCert, err := cert.GetLastCert(n.ctx, n.conn, n.PartyId())
	if err != nil {
		log.Error(err)
		return
	}

	var lastCertHash cert.Hex
	if lastCert != nil {
		lastCertHash = lastCert.CertHash
	} else {
		lastCertHash = cert.GenesisHash()
	}

	hash, blockHash, dbEvents, err := GetTestBlockMTHash(n.conn, n.PartyId(), size)
	if err != nil {
		log.Error(err)
		return
	}

	mcfg := n.config.Mithril
	participants := []mithril.Participant{n.Participant}

	var peerNodes []*PeerNode
	for _, p := range n.peerNodes {
		if p.status != Ready {
			continue
		}
		participants = append(participants, p.participant)
		peerNodes = append(peerNodes, p)
	}

	nextCert := cert.Certificate{
		Id:           uint64(time.Now().Unix()),
		NodeId:       n.PartyId(),
		Participants: participants,
		PrevHash:     lastCertHash,
		BlockNumber:  size,
		BlockHash:    blockHash,
		MerkleRoot:   hash,
		SigStartedAt: time.Now(),
	}
	nextCert.CertHash = cert.Hash(nextCert)

	params := mithril.Parameters{K: mcfg.Params.K, M: mcfg.Params.M, PhiF: mcfg.Params.PhiF}

	n.sigProcess = &sigProcess{
		participants: participants,
		hashStr:      base64.StdEncoding.EncodeToString(hash),
		cert:         nextCert,
		signatures:   make(map[peer.ID][]Signature, 0),
	}

	req := SigRequest{
		RequestId:    n.sigProcess.cert.Id,
		Params:       params,
		Participants: participants,
		Cert:         nextCert,
	}

	log.Infow("Issued TEST multiSig request",
		"id", nextCert.Id,
		"cert_hash", nextCert.CertHash.String(),
		"prev_hash", nextCert.PrevHash.String(),
		"merle_root", nextCert.MerkleRoot.String(),
		"block_number", nextCert.BlockNumber,
		"block_hash", nextCert.BlockHash,
		"participants_amount", len(participants),
	)

	msg := Message{Type: sigRequest, Payload: req}
	for _, p := range peerNodes {
		p.writeCh <- msg
	}

	sigs, err := n.CreateNodeSignatures(req, participants)
	if err != nil {
		return
	}

	sigReqEvent.Stop()
	events := []*bm.Event{sigReqEvent}
	events = append(events, dbEvents...)

	for _, e := range events {
		e.NodeId = n.PartyId()
		e.CertHash = nextCert.CertHash
	}

	err = bm.SaveAll(n.ctx, n.conn, events)
	if err != nil {
		log.Error(err)
	}

	n.sigProcess.signatures[n.host.ID()] = sigs
}

func (n *Node) CreateNodeSignatures(req SigRequest, participants []mithril.Participant) ([]Signature, error) {

	var success uint64 = 0
	indices := make([]uint64, req.Params.K)

	mcfg := n.config.Mithril
	part := mcfg.Participants[mcfg.PartyId]

	initializer := mithril.DecodeInitializer(part.Initializer)
	signer := mithril.NewSigner(initializer, participants)

	certHash := cert.Hash(req.Cert)
	if bytes.Compare(certHash, req.Cert.CertHash) != 0 {
		return nil, errors.New("hashes are not match")
	}

	var i uint64
	for i = 0; i < req.Params.M && success < req.Params.K; i++ {
		if signer.EligibilityCheck(i, req.Cert.CertHash.String()) {
			indices[success] = i
			success++
		}
	}

	if success < req.Params.K {
		return nil, errors.New("not enough indices for the sig")
	}

	var signatures []Signature
	for i = 0; i < req.Params.K; i++ {
		sign, err := signer.Sign(indices[i], req.Cert.CertHash.String())
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
		"cert_id", req.Cert.Id,
		"cert_hash", req.Cert.CertHash.String(),
		"prev_hash", req.Cert.PrevHash.String(),
		"merle_root", req.Cert.MerkleRoot.String(),
		"block_number", req.Cert.BlockNumber,
		"block_hash", req.Cert.BlockHash,
	)

	var hash []byte
	var err error
	var dbEvents []*bm.Event
	if bytes.HasPrefix(req.Cert.BlockHash, []byte{0x0, 0x0, 0x0, 0x0}) {
		hash, _, dbEvents, err = GetTestBlockMTHash(n.conn, 0, req.Cert.BlockNumber)
	} else {
		hash, _, err = GetBlockMTHash(n.conn, req.Cert.BlockNumber)
	}

	if err != nil {
		log.Error(err)
		return
	}

	if bytes.Compare(hash, req.Cert.MerkleRoot) != 0 {
		log.Infow("MT hash didn't match",
			"req_merkle_root", req.Cert.MerkleRoot,
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
		"block_number", req.Cert.BlockNumber,
		"hash", req.Cert.MerkleRoot,
	)

	for _, e := range dbEvents {
		e.NodeId = n.PartyId()
		e.CertHash = req.Cert.CertHash
	}

	err = bm.SaveAll(n.ctx, n.conn, dbEvents)
	if err != nil {
		log.Error(err)
	}
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

	mcfg := n.config.Mithril
	part := mcfg.Participants[mcfg.PartyId]
	initializer := mithril.DecodeInitializer(part.Initializer)

	signer := mithril.NewSigner(initializer, n.sigProcess.participants)
	clerk := signer.Clerk()

	for _, s := range res.Signatures {
		sig := mithril.DecodeSignature(s.Sig, s.Index)
		err := clerk.VerifySign(n.sigProcess.cert.CertHash.String(), s.Index, sig)
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

	aggEvent := bm.New("Aggregate MultiSig")
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

	multiSig, err := clerk.Aggregate(sigs, n.sigProcess.cert.CertHash.String())
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

	err = clerk.VerifyMultiSign(multiSig, n.sigProcess.cert.CertHash.String())
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

	if bytes.HasPrefix(n.sigProcess.cert.BlockHash, []byte{0x0, 0x0, 0x0, 0x0}) {
		n.nextTC <- true
	}

	if err != nil {
		log.Errorw("Failed to save cert to DB", "err", err)
		return
	}

	log.Infow("MultiSig has been aggregated")

	aggEvent.Stop()
	aggEvent.NodeId = n.PartyId()
	aggEvent.CertHash = n.sigProcess.cert.CertHash

	err = bm.Save(n.ctx, n.conn, aggEvent)
	if err != nil {
		log.Error(err)
	}
}

func (n *Node) GetParticipant(ctx context.Context) mithril.Participant {
	return n.Participant
}

func (n *Node) GetPeers(ctx context.Context) []mithril.Participant {
	// TODO(illia-korotia): use RWMutex
	n.peerLock.Lock()
	defer n.peerLock.Unlock()
	p := make([]mithril.Participant, 0, len(n.peerNodes))
	for _, v := range n.peerNodes {
		p = append(p, v.participant)
	}
	return p
}

func (n *Node) GetParams(ctx context.Context) mithril.Parameters {
	return mithril.Parameters(n.config.Mithril.Params)
}
