package main

import (
	"fmt"
	"github.com/input-output-hk/mithril/go-node/pkg/config"
)

func main() {

	cfg, err := config.ReadConfigFromFile("dev-config")
	if err != nil {
		panic(err)
	}

	fmt.Println(cfg)
}
