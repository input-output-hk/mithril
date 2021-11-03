package main

import (
	"context"
	"github.com/input-output-hk/mithril/go-node/pkg/config"
	"github.com/input-output-hk/mithril/go-node/pkg/node"
)

func main() {

	cfg, err := config.ReadConfigFromFile("dev-config")
	if err != nil {
		panic(err)
	}

	p2pNode, err := node.New(context.Background(), cfg)
	if err != nil {
		panic(err)
	}

	_ = p2pNode.ServeNode()
}
