package client

import "github.com/input-output-hk/mithril/mithril-proto/mithril-node-poc/pkg/mithril"

type (
	NetworkConfig struct {
		Params mithril.Parameters     `json:"params"`
		Peers  []*mithril.Participant `json:"peers"`
	}
)
