package main

import (
	"context"
	"fmt"
	"github.com/input-output-hk/mithril/go-node/internal/pg"
	"github.com/input-output-hk/mithril/go-node/pkg/config"
	"github.com/input-output-hk/mithril/go-node/pkg/node"
)

func main() {

	cfg, err := config.ReadConfigFromFile("dev-config")
	if err != nil {
		panic(err)
	}

	fmt.Println("foo", cfg.PostgresDSN)
	dbConn, err := pg.NewConn(context.Background(), cfg.PostgresDSN)
	if err != nil {
		panic(err)
	}

	p2pNode, err := node.New(context.Background(), cfg, dbConn)
	if err != nil {
		panic(err)
	}

	_ = p2pNode.ServeNode()
}
