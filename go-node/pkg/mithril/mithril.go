package mithril

/*
#cgo CFLAGS: -g -Wall
#cgo LDFLAGS: -L. -lmithril
#include "mithril.h"
*/
import "C"

import (
	"encoding/base64"
	"errors"
)

type (
	StmParameters = C.StmParameters

	Participant struct {
		PartyId   uint64
		Stake     uint64
		PublicKey string
		pk        C.MspPkPtr
	}

	Parameters struct {
		K, M uint64
		PhiF float64
	}
)

var (
	ErrSignFailed          = errors.New("sign failed")
	ErrVerifyFailed        = errors.New("verification failed")
	ErrNotEnoughSignatures = errors.New("not enough signatures")

	ErrLotteryLost       = errors.New("lottery lost")
	ErrInvalidMerkleTree = errors.New("invalid merkle tree")
	ErrInvalidSignature  = errors.New("invalid signature")

	ErrInvalidAggregate = errors.New("invalid aggregate")
	ErrProofError       = errors.New("proof error")
)

func NewStmtParams(k, m uint64, phiF float64) StmParameters {
	return StmParameters{
		k:     C.ulonglong(k),
		m:     C.ulonglong(m),
		phi_f: C.double(phiF),
	}
}

func NewParticipant(partyId, stake uint64, key string) Participant {
	keyBytes, err := base64.StdEncoding.DecodeString(key)
	if err != nil {
		panic(err)
	}

	return Participant{
		PartyId:   partyId,
		Stake:     stake,
		PublicKey: key,
		pk:        decodePublicKey(keyBytes),
	}
}
