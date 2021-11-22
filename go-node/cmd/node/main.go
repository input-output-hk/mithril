package main

import (
	"context"
	"github.com/input-output-hk/mithril/go-node/internal/pg"
	"github.com/input-output-hk/mithril/go-node/pkg/api"
	"github.com/input-output-hk/mithril/go-node/pkg/config"
	"github.com/input-output-hk/mithril/go-node/pkg/node"
	"os"
	"os/signal"
	"sync"
	"syscall"
	"time"
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

	ctx, cancel := context.WithCancel(context.Background())

	p2pNode, err := node.New(ctx, cfg, dbConn)
	if err != nil {
		panic(err)
	}

	// start api server goroutine
	apiServer, err := api.NewServer(cfg, dbConn)
	if err != nil {
		panic(err)
	}

	wg := sync.WaitGroup{}

	// start node goroutine
	wg.Add(1)
	go func() {
		_ = p2pNode.ServeNode()
		wg.Done()
	}()

	// start api server
	wg.Add(1)
	go func() {
		_ = apiServer.ListenAndServe()
		wg.Done()
	}()

	sigCh := make(chan os.Signal)
	signal.Notify(sigCh, syscall.SIGKILL, syscall.SIGINT)

	<-sigCh

	cancel()
	exitCtx, exitCancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer exitCancel()

	go func() { _ = apiServer.Shutdown(exitCtx) }()
	go func() { _ = p2pNode.Shutdown() }()

	wg.Wait()
}
