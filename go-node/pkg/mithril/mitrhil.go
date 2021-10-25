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
	Participant   = C.Participant
)

var (
	ErrSignFailed          = errors.New("sign failed")
	ErrVerifyFailed        = errors.New("verification failed")
	ErrNotEnoughSignatures = errors.New("not enough signatures")
)

func NewStmtParams(m, k int, phiF float64) StmParameters {
	return StmParameters{
		m:     C.ulonglong(m),
		k:     C.ulonglong(k),
		phi_f: C.double(phiF),
	}
}

func NewParticipant(partyId, stake int64) Participant {
	k := C.Participant{
		party_id: C.PartyId(partyId),
		stake:    C.Stake(stake),
	}
	return k
}
