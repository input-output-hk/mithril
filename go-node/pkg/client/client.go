package client

import (
	"fmt"
	"github.com/Pantani/request"
	"github.com/input-output-hk/mithril/go-node/pkg/cardano/types"
	"github.com/input-output-hk/mithril/go-node/pkg/cert"
	"github.com/input-output-hk/mithril/go-node/pkg/mithril"
	"github.com/pkg/errors"
	"strings"
)

func NewClient(url string) MithrilClient {
	url = strings.TrimRight(url, "/")
	return MithrilClient{httpClient: request.InitClient(url)}
}

type MithrilClient struct {
	url        string
	httpClient request.Request
}

func (mc MithrilClient) Config() (*NetworkConfig, error) {
	var nc NetworkConfig
	err := mc.httpClient.Get(&nc, "config", nil)
	if err != nil {
		return nil, err
	}

	for _, p := range nc.Peers {
		if err := mithril.DecodeParticipant(p); err != nil {
			return nil, err
		}
	}

	return &nc, nil
}

func (mc MithrilClient) ParticipantsByPartyId() (map[uint64]*mithril.Participant, error) {
	nc, err := mc.Config()
	if err != nil {
		return nil, err
	}

	participantsMap := make(map[uint64]*mithril.Participant, 0)
	for _, p := range nc.Peers {
		participantsMap[p.PartyId] = p
	}

	return participantsMap, nil
}

func (mc MithrilClient) ClerkForCertificate(c cert.Certificate) (mithril.Clerk, error) {
	nc, err := mc.Config()
	if err != nil {
		return mithril.Clerk{}, err
	}

	participantsMap := make(map[uint64]*mithril.Participant, 0)
	for _, p := range nc.Peers {
		participantsMap[p.PartyId] = p
	}

	var participants []mithril.Participant
	for _, pe := range c.Participants {
		if p, ok := participantsMap[pe.PartyId]; ok {
			participants = append(participants, mithril.NewParticipant(p.PartyId, pe.Stake, p.PublicKey))
		} else {
			return mithril.Clerk{}, errors.Errorf("party_id=%d is not found", pe.PartyId)
		}
	}

	clerk := mithril.NewClerk(nc.Params, participants)
	return clerk, nil
}

func (mc MithrilClient) RecentCerts() ([]cert.Certificate, error) {
	var certs []cert.Certificate
	err := mc.httpClient.Get(&certs, "certs", nil)
	if err != nil {
		return nil, err
	}
	return certs, nil
}

func (mc MithrilClient) CertByHash(hash string) (*cert.Certificate, error) {
	var c cert.Certificate
	err := mc.httpClient.Get(&c, fmt.Sprintf("certs/%s", hash), nil)
	if err != nil {
		return nil, err
	}
	return &c, nil
}

func (mc MithrilClient) UTXO(mt string) ([]types.UTXO, error) {
	var utxos []types.UTXO
	err := mc.httpClient.Get(&utxos, fmt.Sprintf("utxo/%s", mt), nil)
	if err != nil {
		return nil, err
	}
	return utxos, nil
}
