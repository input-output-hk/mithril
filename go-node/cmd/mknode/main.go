package main

import (
	"encoding/base64"
	"fmt"
	"github.com/input-output-hk/mithril/go-node/pkg/mithril"
	"os"
	"strconv"
)

func main() {
	if len(os.Args) != 6 {
		fmt.Printf("Usage: %s <k> <m> <phi> <party_id> <stake>", os.Args[0])
		return
	}

	var errs []error

	k, err := strconv.ParseInt(os.Args[1], 10, 64)
	errs = append(errs, err)

	m, err := strconv.ParseInt(os.Args[2], 10, 64)
	errs = append(errs, err)

	phi, err := strconv.ParseFloat(os.Args[3], 64)
	errs = append(errs, err)

	partyId, err := strconv.ParseInt(os.Args[4], 10, 64)
	errs = append(errs, err)

	stake, err := strconv.ParseInt(os.Args[5], 10, 64)
	errs = append(errs, err)

	for i, err := range errs {
		if err != nil {
			fmt.Printf("Invalid arg %d: %s\n", i, err.Error())
			return
		}
	}

	fmt.Println("Input args:", k, m, phi, partyId, stake)

	params := mithril.Parameters{K: uint64(k), M: uint64(m), PhiF: phi}
	initializer, err := mithril.NewInitializer(params, uint64(partyId), uint64(stake))
	participant := initializer.Participant()

	fmt.Printf("Initializer: part_id=%d, stake=%d\n", initializer.PartyId(), initializer.Stake())
	fmt.Println("Participant:", participant.PartyId, participant.Stake)
	fmt.Println(base64.StdEncoding.EncodeToString(initializer.Encode()))
}
