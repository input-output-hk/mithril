package api

import (
	"context"
	"encoding/hex"
	"github.com/input-output-hk/mithril/go-node/pkg/config"
	"net/http"

	"github.com/go-chi/chi"
	"github.com/input-output-hk/mithril/go-node/internal/pg"
	"github.com/input-output-hk/mithril/go-node/pkg/cardano"
	"github.com/input-output-hk/mithril/go-node/pkg/cardano/mt"
	"github.com/input-output-hk/mithril/go-node/pkg/cardano/types"
	"github.com/input-output-hk/mithril/go-node/pkg/cert"
	"github.com/input-output-hk/mithril/go-node/pkg/mithril"
	"github.com/jackc/pgx/v4"
)

type Proof struct {
	Hashes []string `json:"hashes"`
	Index  uint64   `json:"index"`
}

type ProofDAO struct {
	Proof Proof      `json:"proof"`
	UTxO  types.UTXO `json:"utxo"`
}

type network struct {
	Params      mithril.Parameters     `json:"params"`
	CurrentNode mithril.Participant    `json:"currentNode"`
	Peers       []*mithril.Participant `json:"peers"`
}

func listCertificates(w http.ResponseWriter, r *http.Request) {
	n := GetNode(r)
	var certs []cert.Certificate

	err := pg.WithTX(r.Context(), GetDbConn(r), func(ctx context.Context, tx pgx.Tx) error {
		var err error
		certs, err = cert.Recent(ctx, tx, n.PartyId())
		return err
	})
	if err != nil {
		ErrResponse(w, err)
		return
	}

	JsonResponse(w, 200, certs)
}

func utxo(w http.ResponseWriter, r *http.Request) {
	hash, err := hex.DecodeString(chi.URLParam(r, "hash"))
	if err != nil {
		ErrResponse(w, err)
		return
	}

	var certificate *cert.Certificate
	var utxoSet []*types.UTXO

	err = pg.WithTX(r.Context(), GetDbConn(r), func(ctx context.Context, tx pgx.Tx) error {
		var err error
		n := GetNode(r)
		certificate, err = cert.GetByCertHash(ctx, tx, n.PartyId(), hash)
		if err != nil {
			return err
		}

		utxoSet, err = cardano.GetAllTxOutputs(ctx, tx, certificate.BlockNumber)
		return err
	})

	if err != nil {
		ErrResponse(w, err)
		return
	}

	JsonResponse(w, http.StatusOK, utxoSet)
}

func utxoByAddr(w http.ResponseWriter, r *http.Request) {
	hash, err := hex.DecodeString(chi.URLParam(r, "merkle_root"))
	if err != nil {
		ErrResponse(w, err)
		return
	}

	addr := chi.URLParam(r, "addr")

	var certificate *cert.Certificate
	tree := mt.NewMerkleTree()
	var utxoSet []types.UTXO

	err = pg.WithTX(r.Context(), GetDbConn(r), func(ctx context.Context, tx pgx.Tx) error {
		var err error
		certificate, err = cert.GetByMerkleTreeHash(ctx, tx, hash)
		if err != nil {
			return err
		}

		_, _, err = cardano.ProcessUTXO(ctx, tx, tree, certificate.BlockNumber)
		if err != nil {
			return err
		}

		utxoSet, err = cardano.GetTxOutputsByAddr(ctx, tx, certificate.BlockNumber, addr)

		return err
	})
	if err != nil {
		ErrResponse(w, err)
		return
	}

	var proofs []ProofDAO
	for _, l := range utxoSet {

		bytes, err := mt.CalculateHash(&l)
		if err != nil {
			ErrResponse(w, err)
			return
		}
		p, err := tree.GetProof(bytes)

		if err != nil {
			ErrResponse(w, err)
			return
		}

		pd := ProofDAO{Proof: Proof{Index: p.Index}, UTxO: l}
		for _, e := range p.Hashes {
			pd.Proof.Hashes = append(pd.Proof.Hashes, hex.EncodeToString(e))
		}
		proofs = append(proofs, pd)
	}

	JsonResponse(w, http.StatusOK, struct {
		Address string     `json:"address"`
		Proofs  []ProofDAO `json:"proofs"`
	}{
		Address: addr,
		Proofs:  proofs,
	})
}

func getConfig(cfg *config.Config) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		mcfg := cfg.Mithril
		params := mcfg.Params

		var participants []*mithril.Participant
		for _, p := range mcfg.Participants {
			initializer, err := mithril.DecodeInitializer(p.Initializer)
			if err != nil {
				ErrResponse(w, err)
				return
			}
			participants = append(participants, initializer.Participant())
		}

		res := network{
			Params: mithril.Parameters{K: params.K, M: params.M, PhiF: params.PhiF},
			//CurrentNode: node.GetParticipant(ctx),
			Peers: participants,
		}
		JsonResponse(w, http.StatusOK, res)
	}
}

// getCertByHash return a certificate by certificate hash.
func getCertByHash(w http.ResponseWriter, r *http.Request) {
	n := GetNode(r)
	hash, err := hex.DecodeString(chi.URLParam(r, "hash"))
	if err != nil {
		ErrResponse(w, err)
		return
	}

	var c *cert.Certificate

	err = pg.WithTX(r.Context(), GetDbConn(r), func(ctx context.Context, tx pgx.Tx) error {
		var err error
		c, err = cert.GetByCertHash(ctx, tx, n.Participant.PartyId, hash)
		return err
	})
	if err != nil {
		ErrResponse(w, err)
		return
	}

	for i := 0; i < len(c.Participants); i++ {
		c.Participants[i].PublicKey = ""
	}

	JsonResponse(w, 200, c)
}

// getAllCerts return all certificates for all nodes.
func getAllCerts(w http.ResponseWriter, r *http.Request) {
	n := GetNode(r)
	var certs []cert.Certificate

	err := pg.WithTX(r.Context(), GetDbConn(r), func(ctx context.Context, tx pgx.Tx) error {
		var err error
		certs, err = cert.GetAllCerts(ctx, tx, n.Participant.PartyId)
		return err
	})
	if err != nil {
		ErrResponse(w, err)
		return
	}

	JsonResponse(w, 200, certs)
}
