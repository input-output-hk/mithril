package mithril

/*
#cgo CFLAGS: -g -Wall
#cgo LDFLAGS: -L. -lmithril -lm -ldl
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
		PartyId   uint64 `mapstructure:"party_id" json:"party_id"`
		Stake     uint64 `mapstructure:"stake" json:"stake"`
		PublicKey string `mapstructure:"public_key" json:"public_key"`
		pk        C.MspPkPtr
	}

	Parameters struct {
		K    uint64  `mapstructure:"k" json:"k"`
		M    uint64  `mapstructure:"m" json:"m"`
		PhiF float64 `mapstructure:"phi_f" json:"phi_f"`
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
