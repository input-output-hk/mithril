package main

import (
	"context"
	"github.com/input-output-hk/mithril/go-node/internal/pg"
	"github.com/input-output-hk/mithril/go-node/pkg/api"
	"github.com/input-output-hk/mithril/go-node/pkg/config"
	"github.com/input-output-hk/mithril/go-node/pkg/node"
	"sync"
)

func main() {

	cfg, err := config.ReadConfigFromFile("dev-config")
	if err != nil {
		panic(err)
	}

	dbConn, err := pg.NewConn(context.Background(), cfg.PostgresDSN)
	if err != nil {
		panic(err)
	}
	defer dbConn.Close(context.TODO())

	err = pg.ApplyMigrations(dbConn)
	if err != nil {
		panic(err)
	}

	p2pNode, err := node.New(context.Background(), cfg, dbConn)
	if err != nil {
		panic(err)
	}

	wg := sync.WaitGroup{}
	wg.Add(1)

	go func() {
		_ = p2pNode.ServeNode()
		wg.Done()
	}()

	if cfg.Mithril.PartyId == 0 {
		apiServer := api.NewServer(cfg, dbConn)
		wg.Add(1)
		go func() {
			_ = apiServer.ListenAndServe()
			wg.Done()
		}()
	}

	wg.Wait()
}
