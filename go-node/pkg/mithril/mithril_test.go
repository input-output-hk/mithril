package mithril

import (
	"testing"
)

func TestRustLibIntegration(t *testing.T) {
	msg := "123"

	p0 := NewParticipant(1, 1)
	keyReg := NewKeyReg([]*Participant{p0})

	params := NewStmtParams(1, 100, 1.0)
	initializer := NewStmInitializer(params)
	//// defer initializer.Free()
	//
	initializer.Register(keyReg)
	initializer.BuildAVK(keyReg)

	signer := initializer.Finish()
	if !signer.EligibilityCheck(1, msg) {
		t.Fail()
	}

	sign, err := signer.Sign(1, msg)
	if err != nil {
		panic(err)
	}

	clerk := signer.GetClerk()
	if !clerk.VerifySign(msg, 1, sign) {
		t.Fatalf("Signature invalid")
	}

	multiSign, err := clerk.Aggregate(1, sign, msg)
	if err != nil {
		t.Fatalf("Falied to aggreate")
	}

	if clerk.VerifyMultiSign(multiSign, msg) {
		clerk.Free()
		sign.Free()
		// initializer.Free()
	}
}