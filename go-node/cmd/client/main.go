package main

import (
	"fmt"
	"github.com/cheynewallace/tabby"
	"github.com/input-output-hk/mithril/go-node/internal/log"
	"github.com/input-output-hk/mithril/go-node/pkg/client"
	"github.com/pkg/errors"
	"github.com/urfave/cli/v2"
	"os"
)

func main() {
	app := cli.NewApp()
	app.Name = "Mithril Client"

	app.Flags = []cli.Flag{
		&cli.StringFlag{
			Name:  "host",
			Value: "http://localhost:8000/",
			Usage: "usage",
		},
	}

	app.Commands = append(app.Commands, &cli.Command{
		Name:   "list",
		Usage:  "list recent certificates",
		Action: list,
	})

	app.Commands = append(app.Commands, &cli.Command{
		Name:   "verify",
		Usage:  "verify multiSig of the certificate",
		Action: verify,
	})

	err := app.Run(os.Args)
	if err != nil {
		log.Fatal(err)
	}
}

func list(c *cli.Context) error {
	mc := client.NewClient(c.String("host"))

	certs, err := mc.RecentCerts()
	if err != nil {
		return err
	}

	t := tabby.New()
	t.AddHeader("ID", "Merkle Root", "Created At")
	for _, c := range certs {
		t.AddLine(c.Id, c.MerkleRoot.String(), c.SigFinishedAt.String())
	}

	t.Print()

	return nil
}

func verify(c *cli.Context) error {

	if c.Args().Len() != 1 {
		fmt.Println("Provide a MerkleRoot hash")
		return nil
	}

	hash := c.Args().Get(0)
	mc := client.NewClient(c.String("host"))

	certs, err := mc.RecentCerts()
	if err != nil {
		return err
	}

	for _, vc := range certs {
		if vc.MerkleRoot.String() == hash {
			err := vc.VerifyMultiSig()
			if err != nil {
				return err
			}
			fmt.Printf("%s: has been signed properly\n", hash)
			return nil
		}
	}

	return errors.Errorf("%s: no certificate has been found", hash)
}
