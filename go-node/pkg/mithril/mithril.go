package mithril

/*
#cgo CFLAGS: -g -Wall
#cgo LDFLAGS: -L. -lmithril
#include "mithril.h"
*/
import "C"

import (
	"errors"
)

type (
	StmParameters = C.StmParameters
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

func NewStmtParams(k, m int, phiF float64) StmParameters {
	return StmParameters{
		k:     C.ulonglong(k),
		m:     C.ulonglong(m),
		phi_f: C.double(phiF),
	}
}
