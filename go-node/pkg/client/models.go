package client

import "github.com/input-output-hk/mithril/go-node/pkg/mithril"

type (
	NetworkConfig struct {
		Params mithril.Parameters    `json:"params"`
		Peers  []mithril.Participant `json:"peers"`
	}
)
