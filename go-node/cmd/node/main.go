package main

import (
	"fmt"
	"github.com/input-output-hk/mithril/go-node/pkg/mithril"
)

func main() {

	msg := "123"
	mithril.NewParticipant(1, 1)

	params := mithril.NewStmtParams(1, 100, 1.0)

	p0 := mithril.NewParticipant(1, 1)
	k := mithril.NewKeyReg([]*mithril.Participant{p0})
	defer k.Free()

	initializer := mithril.NewStmInitializer(params)
	// defer initializer.Free()

	initializer.Register(k)
	initializer.BuildAVK(k)

	signer := initializer.Finish()
	if !signer.EligibilityCheck(1, msg) {
		fmt.Println("Not eligible to sign")
		return
	}

	sign, err := signer.Sign(1, msg)
	if err != nil {
		panic(err)
	}

	clerk := signer.GetClerk()
	// defer clerk.Free()

	if !clerk.VerifySign(msg, 1, sign) {
		fmt.Println("Signature invalid")
		return
	}

	multiSign, err := clerk.Aggregate(1, sign, msg)
	if err != nil {
		panic(err)
	}

	if clerk.VerifyMultiSign(multiSign, msg) {
		fmt.Println("Test completed successfully!")
	} else {
		fmt.Println("Verification of multisignature failed.")
	}
}
