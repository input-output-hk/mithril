package main

import (
	bytes2 "bytes"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"github.com/cheynewallace/tabby"
	"github.com/input-output-hk/mithril/go-node/internal/log"
	mt2 "github.com/input-output-hk/mithril/go-node/pkg/cardano/mt"
	"github.com/input-output-hk/mithril/go-node/pkg/cardano/types"
	"github.com/input-output-hk/mithril/go-node/pkg/cert"
	"github.com/input-output-hk/mithril/go-node/pkg/client"
	"github.com/pkg/errors"
	"github.com/urfave/cli/v2"
	"io"
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
		Name:   "fetch_cert",
		Usage:  "fetch a certificate",
		Action: fetchCert,
	})

	app.Commands = append(app.Commands, &cli.Command{
		Name:   "verify_cert",
		Usage:  "certificate verification",
		Action: verifyCert,
	})

	app.Commands = append(app.Commands, &cli.Command{
		Name:   "fetch_utxo",
		Usage:  "fetch utxo set for a certificate",
		Action: fetchUTXO,
	})

	app.Commands = append(app.Commands, &cli.Command{
		Name:   "verify_utxo",
		Usage:  "verify utxo set within a certificate",
		Action: verifyUTXO,
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
	t.AddHeader("ID", "BlockNum", "Merkle Root", "Created At")
	for _, c := range certs {
		t.AddLine(c.Id, c.BlockNumber, c.MerkleRoot.String(), c.SigFinishedAt.String())
	}

	t.Print()

	return nil
}

func fetchCert(c *cli.Context) error {

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
			data, err := json.MarshalIndent(vc, "", "  ")
			if err != nil {
				return err
			}
			fmt.Println(string(data))
			return nil
		}
	}

	return errors.Errorf("%s: no certificate has been found", hash)
}

func verifyCert(c *cli.Context) error {

	if c.Args().Len() != 1 {
		fmt.Println("Please provide a filename")
		return nil
	}

	f, err := os.Open(c.Args().Get(0))
	if err != nil {
		return err
	}
	defer f.Close()

	bytes, err := io.ReadAll(f)
	if err != nil {
		return err
	}

	var cv cert.Certificate
	err = json.Unmarshal(bytes, &cv)
	if err != nil {
		return err
	}

	err = cv.VerifyMultiSig()
	if err != nil {
		return err
	}

	fmt.Println("MultiSig verification has been passed")
	return nil
}

func fetchUTXO(c *cli.Context) error {

	if c.Args().Len() != 1 {
		fmt.Println("Provide a MerkleRoot hash")
		return nil
	}

	hash := c.Args().Get(0)
	mc := client.NewClient(c.String("host"))

	utxos, err := mc.UTXO(hash)
	if err != nil {
		return err
	}

	data, err := json.MarshalIndent(utxos, "", "  ")
	if err != nil {
		return err
	}
	fmt.Println(string(data))
	return nil
}

func verifyUTXO(c *cli.Context) error {

	if c.Args().Len() != 2 {
		fmt.Println("Usage: verify_utxo <cert.json> <utxo.json>")
		return nil
	}

	crtf, err := os.Open(c.Args().Get(0))
	if err != nil {
		return err
	}
	defer crtf.Close()

	bytes, err := io.ReadAll(crtf)
	if err != nil {
		return err
	}

	var cv cert.Certificate
	err = json.Unmarshal(bytes, &cv)
	if err != nil {
		return err
	}

	err = cv.VerifyMultiSig()
	if err != nil {
		return err
	}

	utxof, err := os.Open(c.Args().Get(1))
	if err != nil {
		return err
	}

	bytes, err = io.ReadAll(utxof)
	if err != nil {
		return err
	}

	var utxos []types.UTXO
	err = json.Unmarshal(bytes, &utxos)
	if err != nil {
		return err
	}

	root := mt2.NewMerkleTree()
	for _, u := range utxos {
		proof, err := mt2.CalculateHash(&u)
		if err != nil {
			return err
		}
		err = root.Add(proof)
		if err != nil {
			return err
		}
	}

	mtHash, err := root.GetRoot()
	if err != nil {
		return err
	}

	fmt.Println(cv.MerkleRoot.String())
	fmt.Println(hex.EncodeToString(mtHash))
	if bytes2.Compare(mtHash, cv.MerkleRoot) == 0 {
		fmt.Println("Verification has been passed")
	}

	return errors.New("verification failed")
}
